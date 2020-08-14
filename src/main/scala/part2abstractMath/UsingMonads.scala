package part2abstractMath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._
  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  // DO NOT CHANGE THE CODE

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M

    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some(s"Request ($payload) has been accepted")
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap {
    conn => OptionHttpService.issueRequest(conn, "Hello, HTTP service")
  }
  val responseOptionFor = for {
    conn <- OptionHttpService.getConnection(config)
    response <- OptionHttpService.issueRequest(conn, "Hello, HTTP service")
  } yield response

  // TODO implement another HttpService with LoadingOr or ErrorOr
  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port"))  {
        Left(new RuntimeException("Connection could not be established: invalid configuration"))
      } else {
        Right(Connection(cfg("host"), cfg("port")))
      }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large"))
      else Right(s"Request ($payload) was accepted")
  }

  val errorOrResponse: ErrorOr[String] = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response

  def main(args: Array[String]): Unit = {
    println(getResponse(OptionHttpService, "Hello Option"))
    println(getResponse(AggressiveHttpService, "Hello, ErrorOr"))
  }
}
