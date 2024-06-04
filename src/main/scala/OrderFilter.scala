import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class Order(customerName: String, customerContact: String, shippingAddress: String, total: Double, date: LocalDate, orderItems: List[Item])
case class Item(cost: Double, shippingFee: Double, taxAmount: Double, product: Product)
case class Product(name: String, category: String, weight: Double, price: Double, date: LocalDate)

class GroupByDateOrderFilter(orders: List[Order], start: LocalDate, end: LocalDate, ranges: Seq[String] = Seq()) {

  private val startEndDiffInMonths: Long = ChronoUnit.MONTHS.between(start, end)
  private val productOrders: Map[(Long, Long), Seq[(LocalDate, Order)]] = filterAndGroupByDate

  private def filterAndGroupByDate: Map[(Long, Long), Seq[(LocalDate, Order)]] = {
    val productOrders: Seq[(LocalDate, Order)] = for {
      order <- orders
      item  <- order.orderItems
    } yield (item.product.date, order)

    val filteredProducts = productOrders.filterNot(entry => entry._1.isBefore(start))
    if (ranges.isEmpty) {
      filteredProducts
        .groupBy(entry => {
          val monthsAfterStart = ChronoUnit.MONTHS.between(start, entry._1)
          if (monthsAfterStart > startEndDiffInMonths) (startEndDiffInMonths + 1, startEndDiffInMonths + 1)
          else if (monthsAfterStart == 0) (0L, 1L)
          else (monthsAfterStart - 1, monthsAfterStart)
        })
    } else {
      val parsedRanges: Seq[Array[Int]] = ranges.filterNot(_.contains('>')).map(range => range.split('-').map(_.toInt))
      val parsedGreaterThanRange: Option[Int] = ranges.find(_.contains('>')).map(range => range.split('>')(1).toInt)
      filteredProducts
        .groupBy(entry => {
          val monthsAfterStart = ChronoUnit.MONTHS.between(start, entry._1)
          val range = parsedRanges.find(range => monthsAfterStart >= range(0) && monthsAfterStart < range(1)).getOrElse(Array(-1, -1))
          if (parsedGreaterThanRange.exists(monthsAfterStart > _)) (parsedGreaterThanRange.get, Int.MaxValue)
          else (range(0), range(1))
        })
    }
  }

  def prettyPrint(): Unit = {
    productOrders.toList.sortBy(entry => entry._1._1)
      .filterNot(_._1._1 < 0)
      .foreach(entry => {
        if (entry._1._2 == Int.MaxValue) println(s"> ${entry._1._1} months -> " + entry._2.size + " orders")
        else if (entry._1._1 > startEndDiffInMonths) println(s"> $startEndDiffInMonths months -> " + entry._2.size + " orders")
        else println(entry._1._1 + "-" + entry._1._2 + " months -> " + entry._2.size + " orders")
      })
  }
}

object OrderFilter {

  def main(args: Array[String]): Unit = {
    val (start, end, ranges) = args.toList match {
      case List(start, end) => (LocalDate.parse(start), LocalDate.parse(end), Seq())
      case start :: end :: ranges => (LocalDate.parse(start), LocalDate.parse(end), ranges)
    }

    val now = LocalDate.now()

    val orders = List(
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start)))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(1))))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(1))))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(2))))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(3))))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(4))))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(15))))),
      Order("customer", "123", "stree", 10.0, now, List(Item(1.1, 1.1, 1.1, Product("p", "c", 2.2, 2.2, start.plusMonths(16))))),
    )

    val groupByDateOrderFilter = new GroupByDateOrderFilter(orders, start, end, ranges)
    groupByDateOrderFilter.prettyPrint()

  }
}
