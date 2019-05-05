import java.io.FileOutputStream

import scala.io.{Codec, Source}

object EmptyRow {
  def unapply(row: String): Option[String] = Some(row).filter(_.trim.isEmpty)
}

case class Client(name: String,
                  balanceUSD: Int,
                  balanceA: Int,
                  balanceB: Int,
                  balanceC: Int,
                  balanceD: Int) {
  override def toString: String = s"$name\t$balanceUSD\t$balanceA\t$balanceB\t$balanceC\t$balanceD"
}

case object Client {
  def unapply(row: String): Option[Client] = Some(row.split('\t'))
    .map(r => Client(
      r(0).trim,
      r(1).trim.toInt,
      r(2).trim.toInt,
      r(3).trim.toInt,
      r(4).trim.toInt,
      r(5).trim.toInt
    ))
}

object TypeOperation extends Enumeration {
  type TypeOperation = Value

  val Sell, Buy = Value

  def apply(v: String): Value = v match {
    case "s" => Sell
    case "b" => Buy
    case _ => throw new Exception(s"Invalid Operation Type $v")
  }
}

case class Order(clientName: String,
                 operation: TypeOperation.TypeOperation,
                 securityPaper: String,
                 cost: Int,
                 amount: Int) {
  def totalCost = cost * amount * operationSign

  def amountWithSign = amount * operationSign

  def operationSign = operation match {
    case TypeOperation.Sell => 1
    case TypeOperation.Buy => -1
  }
}

case object Order {
  def unapply(row: String): Option[Order] = Some(row.split('\t'))
    .map(r => Order(
      r(0).trim,
      TypeOperation(r(1).trim),
      r(2).trim,
      r(3).trim.toInt,
      r(4).trim.toInt
    ))
}

object MainApp extends App {

  implicit val textEncoding = Codec.UTF8

  def getClients: List[Client] = Source.fromFile("clients.txt").getLines.flatMap{
    case EmptyRow(_) => None
    case Client(client) => Some(client)
  } toList

  def getOrders: List[Order] = Source.fromFile("orders.txt").getLines.flatMap{
    case EmptyRow(_) => None
    case Order(order) => Some(order)
  } toList

  def makeOperation(client: Client, order: Order): Client = order.securityPaper match {
    case _ if client.balanceUSD + order.totalCost < 0 => client

    case "A" if client.balanceA + order.amountWithSign >= 0 =>
      Client(client.name, client.balanceUSD + order.totalCost, client.balanceA + order.amountWithSign, client.balanceB, client.balanceC, client.balanceD)

    case "B" if client.balanceB + order.amountWithSign >= 0 =>
      Client(client.name, client.balanceUSD + order.totalCost, client.balanceA, client.balanceB + order.amountWithSign, client.balanceC, client.balanceD)

    case "C" if client.balanceC + order.amountWithSign >= 0 =>
      Client(client.name, client.balanceUSD + order.totalCost, client.balanceA, client.balanceB, client.balanceC + order.amountWithSign, client.balanceD)

    case "D" if client.balanceD + order.amountWithSign >= 0 =>
      Client(client.name, client.balanceUSD + order.totalCost, client.balanceA, client.balanceB, client.balanceC, client.balanceD + order.amountWithSign)

    case _ => client
  }

  val initialClients = getClients

  val results = getOrders.foldLeft(initialClients) { (clients, order) =>
    clients.map {
      case client if client.name == order.clientName => makeOperation (client, order)
      case client => client
    }
  }

  val resultsFile = new FileOutputStream("results.txt")

  resultsFile.write( results.mkString("\r\n").getBytes )
  resultsFile.flush
  resultsFile.close

}
