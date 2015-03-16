# STILTS |\
STILTS stands for ***SQL that is largely type safe***.

STILTS embeds *real* SQL syntax into Scala that is fully integrated into
Scala's type system. If it compiles, you can expect to avoid the runtime
errors (and security concerns) that plague string based querying.

STILTS is *not* an ORM. ORMs aim to abstract away the underlying database
schema and hide queries behind host language syntax. This can make it hard
to migrate schemas and optimize queries. By obscuring your view of the
underlying database mechanisms, ORMs make the job of writing efficient,
scalable database queries harder, not easier.

## Schema definition syntax

To start, STILTS needs to know the schema of your database. You can
describe a preexisting schema or define new tables using STILTS. The
table definition sytax should look familiar. It maps closely to the
sytax of a `CREATE TABLE` statement. In addition to providing a ready
definition for constructing the table, `Table` objects give a typed
representation of your table for constructing queries.

```scala
object prices extends Table {
  val item_id: Column[Long] = BIGINT.AUTO_INCREMENT.PRIMARY_KEY
  val item_name: Column[String] = VARCHAR(32)
  val item_price: Column[Int] = INT // in US cents
}

object sales extends Table {
  val time_of_sale = TIMESTAMP
  val item_sold = BIGINT.REFERENCES(prices.item_id)
  val number_sold = INT
}
```

## Query syntax

Tables can be created, inserted into, or queried from. It's just SQL.
You already know how to use STILTS. Just type your query and see if
it compiles.

```scala
CREATE.TABLE(prices).exec
CREATE.TABLE(sales).exec

val pencilID = INSERT.INTO(prices)
  .SET(prices.item_name := "pencil", prices.item_price := 299).key[Long]
INSERT.INTO(sales).SET(sales.item_id := pencilID, sales.number_sold := 3).exec
INSERT.INTO(sales).SET(sales.item_id := pencilID, sales.number_sold := 2).exec
UPDATE(prices).SET(prices.item_price := 199).exec

val totalRevenue = SELECT(SUM(sales.number_sold * prices.item_price)).FROM(sales)
  .INNER_JOIN(prices).ON(sales.item_id === prices.item_id).only
assert(totalRevenue === Some(199 * 5))
```

## Does this really work?

STILTS is in production at Clinkle. For our tech stack (MySQL 5.5/5.6, JDBC,
Scala 2.11) and our use cases it works great. Features have been added
as we've needed them. If there's something missing that you want, open
an issue or submit a pull request. It can potentially be adapted to other SQL
databases. Happy forking.
