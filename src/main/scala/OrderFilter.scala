import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class Order(customerName: String, customerContact: String, shippingAddress: String, total: Double, date: LocalDate, orderItems: List[Item])
case class Item(cost: Double, shippingFee: Double, taxAmount: Double, product: Product)
case class Product(name: String, category: String, weight: Double, price: Double, date: LocalDate)

object OrderFilter extends App {
  def filter(orders: List[Order], start: LocalDate, end: LocalDate): Map[(Long, Long), List[Order]] = {
    orders
      .filterNot(order => order.orderItems.minBy(_.product.date).product.date.isBefore(start))
      .groupBy(order => {
        val monthsAfterStart = ChronoUnit.MONTHS.between(start, order.orderItems.minBy(_.product.date).product.date)
        val startEndDiffInMonths = ChronoUnit.MONTHS.between(start, end)
        if (monthsAfterStart > startEndDiffInMonths) (-1, -1)
        else if (monthsAfterStart == 0) (0L, 1L)
        else (monthsAfterStart - 1, monthsAfterStart)
      })
  }

  def prettyPrint(orderMap: Map[(Long, Long), List[Order]], end: Int): Unit = {
    orderMap.toList.sortBy(entry => entry._1._1)
      .foreach(entry => {
        if (entry._1._1 < 0) println(s"> $end months -> " + entry._2.size + " orders")
        else println(entry._1._1 + "-" + entry._1._2 + " months -> " + entry._2.size + " orders")
      })
  }

  val now = LocalDate.now()
  val orders = List(
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now)))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(1))))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(1))))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(2))))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(3))))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(4))))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(15))))),
    Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, now.plusMonths(16))))),
  )

  val result = filter(orders, now, now.plusMonths(12))
  prettyPrint(result, 12)
}
