package com.github.ghengeveld.prerender

import akka.actor._
import spray.client.pipelining._
import spray.http.HttpHeaders._
import spray.http.MediaTypes._
import spray.http._
import spray.routing._
import spray.routing.directives._

import scala.concurrent._
import scala.util.Success

trait PrerenderDirectives {
  import BasicDirectives._
  import ExecutionDirectives._
  import RespondWithDirectives._

  val DEFAULT_CRAWLER_USER_AGENTS = Seq(
    "googlebot", "yahoo", "bingbot", "baiduspider",
    "facebookexternalhit", "twitterbot", "rogerbot",
    "linkedinbot", "embedly"
  )

  val DEFAULT_EXTENSIONS_TO_IGNORE = Seq(
    ".js", ".css", ".less", ".png", ".jpg", ".jpeg",
    ".gif", ".pdf", ".doc", ".txt", ".zip", ".mp3", ".rar", ".exe", ".wmv", ".doc", ".avi", ".ppt", ".mpg",
    ".mpeg", ".tif", ".wav", ".mov", ".psd", ".ai", ".xls", ".mp4", ".m4a", ".swf", ".dat", ".dmg",
    ".iso", ".flv", ".m4v", ".torrent"
  )

  def routePrerender(
    serverHost: String,
    actorRefFactory: ActorRefFactory,
    crawlerUserAgents: Seq[String] = DEFAULT_CRAWLER_USER_AGENTS,
    extensionToIgnore: Seq[String] = DEFAULT_EXTENSIONS_TO_IGNORE
  ) = {
    PrerenderConfiguration(
      serverHost,
      crawlerUserAgents,
      extensionToIgnore,
      actorRefFactory.dispatcher,
      sendReceive(actorRefFactory, actorRefFactory.dispatcher)
    )
  }

  def prerender(implicit config: PrerenderConfiguration): Directive0 = mapInnerRoute { route =>
    implicit ctx =>
      val request = ctx.request
      if (!isGetMethod(request) || isResource(request) || !isCrawler(request)) {
        route(ctx)
      } else {
        respondWithMediaType(`text/html`) {
          detach(config.executionContext) {
            completeFromPrerender
          }
        }(ctx)
      }
  }

  private def completeFromPrerender(ctx: RequestContext)(implicit config: PrerenderConfiguration) {
    implicit val executionContext = config.executionContext
    val prerenderUrl = config.basePath + ctx.request.uri.toString()
    config.pipeline(Get(prerenderUrl)).onComplete {
      case Success(HttpResponse(StatusCodes.OK, entity, _, _)) if entity.nonEmpty =>
        ctx.complete(entity.asString)
      case result =>
        ctx.complete(StatusCodes.InternalServerError -> result.toString)
    }
  }

  private def isGetMethod(request: HttpRequest)(implicit config: PrerenderConfiguration) = request.method == HttpMethods.GET

  private def isCrawler(request: HttpRequest)(implicit config: PrerenderConfiguration) = request.headers.exists {
    case ua: `User-Agent` => ua.products.exists(p => config.crawlerUserAgents.contains(p.product.toLowerCase))
    case _                => false
  }

  private def isResource(request: HttpRequest)(implicit config: PrerenderConfiguration) = {
    val uri = request.uri.toString().toLowerCase
    config.extensionToIgnore.exists(uri.endsWith)
  }
}

object PrerenderDirectives extends PrerenderDirectives

case class PrerenderConfiguration(
  serverHost: String,
  crawlerUserAgents: Seq[String],
  extensionToIgnore: Seq[String],
  executionContext: ExecutionContext,
  pipeline: HttpRequest => Future[HttpResponse]
) {
  val basePath = "http://" + serverHost + "/"
}
