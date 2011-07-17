package unfiltered.request


object Provides {
  trait Providing {
    val contentType: String

    val ext: String

    def unapply[T](r: HttpRequest[T]) = {
      val pathSuffix = r.uri.split("[.]").lastOption
      r match {
        case RequestContentType(value) =>
          if(value.equalsIgnoreCase(contentType))
            Some(r)
          else if (value == "*/*" && pathSuffix.exists { ext == _ })
            Some(r)
          else None
        case _ => pathSuffix match {
          case Some(pathSuffix) if(pathSuffix == ext) => Some(r)
          case _ => None
        }
      }
    }
  }

  object Json extends Providing {
    val contentType = "application/json"
    val ext = "json"
  }

  object Xml extends Providing {
    val contentType = "text/xml"
    val ext = "xml"
  }

  object Html extends Providing {
    val contentType = "text/html"
    val ext = "html"
  }

  object Csv extends Providing {
    val contentType = "text/csv"
    val ext = "csv"
  }
}