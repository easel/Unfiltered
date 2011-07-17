package unfiltered.request

import org.specs._

object ProvidesSpecJetty extends unfiltered.spec.jetty.Served with ProvidesSpec {
  def setup = { _.filter(unfiltered.filter.Planify(intent)) }
}
object ProvidesSpecNetty extends unfiltered.spec.netty.Served with ProvidesSpec {
  def setup = { p =>
    unfiltered.netty.Http(p).handler(unfiltered.netty.cycle.Planify(intent))
  }
}

trait ProvidesSpec extends unfiltered.spec.Hosted {

  import unfiltered.response._
  import unfiltered.request._
  import unfiltered.request.{Path => UFPath}

  import dispatch._

  def intent[A, B]: unfiltered.Cycle.Intent[A, B] = {
    case GET(UFPath(Seg(ext :: Nil)) & Provides.Json(_)) => ResponseString("json")
    case GET(UFPath(Seg(ext :: Nil)) & Provides.Xml(_)) => ResponseString("xml")
    case GET(UFPath(Seg(ext :: Nil)) & Provides.Csv(_)) => ResponseString("csv")
    case GET(UFPath(Seg(ext :: Nil)) & Provides.Html(_)) => ResponseString("html")
  }

  "Provides" should {
    "match an application/json content-type request as json" in {
      val resp = Http(host / "test" <:< Map("Content-Type" -> "application/json") as_str)
      resp must_== "json"
    }
    "match an text/xml content-type request as xml" in {
      val resp = Http(host / "test" <:< Map("Content-Type" -> "text/xml") as_str)
      resp must_== "xml"
    }
    "match an text/csv content-type request as csv" in {
      val resp = Http(host / "test" <:< Map("Content-Type" -> "text/csv") as_str)
      resp must_== "csv"
    }
    "match an application/json content-type request as html" in {
      val resp = Http(host / "test" <:< Map("Content-Type" -> "text/html") as_str)
      resp must_== "html"
    }
  }
}



