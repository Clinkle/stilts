package com.clinkle.test.sqldsl

import java.sql.{PreparedStatement, ResultSet, DriverManager}

import com.clinkle.sql.CREATE.{utf8mb4_unicode_ci, utf8mb4}
import com.clinkle.sql.{ALTER, UPDATE, Executor, USE, Database, Expr, INSERT, DROP, CREATE, SELECT, Table}
import org.scalatest.{BeforeAndAfterAll, FunSpec}

class StarterSpec extends FunSpec with BeforeAndAfterAll {
  import com.clinkle.sql.Primitive._
  import Expr._
  import Executor._

  Class.forName("com.mysql.jdbc.Driver")
  implicit val conn = DriverManager.getConnection("jdbc:mysql://localhost/", "root", "root")

  case class Count(count: Int)

  implicit object CountIsPrimitive extends IntPrimitive[Count] {
    override def setParam(stmt: PreparedStatement, index: Int, value: Count): Unit = IntIsPrimitive.setParam(stmt, index, value.count)
    override def extract(rows: ResultSet, index: Int): Count = Count(IntIsPrimitive.extract(rows, index))
  }

  object TEST_DB extends Database

  object Tab extends Table {
    val col1 = INT[Count]
    val col2 = CHAR(12)

    PRIMARY_KEY(col2)
  }

  object Tab2 extends Table {
    val col1 = INT[Count].REFERENCES(Tab.col1)
    val col2 = SMALLINT
    val col3 = VARCHAR(2)
  }

  object prices extends Table {
    val item_id = BIGINT.AUTO_INCREMENT.PRIMARY_KEY
    val item_name = VARCHAR(32)
    val item_price = INT[Int]
  }

  object sales extends Table {
    val time_of_sale = TIMESTAMP
    val item_id = BIGINT.REFERENCES(prices.item_id)
    val number_sold = INT[Int]
  }

  override def beforeAll() {
    DROP.DATABASE.IF_EXISTS(TEST_DB).exec
    CREATE.DATABASE(TEST_DB).DEFAULT_CHARACTER_SET(utf8mb4).DEFAULT_COLLATE(utf8mb4_unicode_ci).exec
    USE(TEST_DB).exec

    CREATE.TABLE(Tab).exec
    CREATE.TABLE(Tab2).exec
  }

  describe("Test tables") {
    it("are created successfully") {
      val expectedC1 = Count(1)
      val expectedC2 = "abcdefhhijkl"

      INSERT.INTO(Tab).SET(Tab.col1 := expectedC1, Tab.col2 := expectedC2).exec
      val (c1: Count, c2: String) = SELECT(Tab.col1, Tab.col2).FROM(Tab).LIMIT(1).only

      assert(c1 === expectedC1)
      assert(c2 === expectedC2)
    }

    it("allow for a more complex query example") {
      CREATE.TABLE(prices).exec
      CREATE.TABLE(sales).exec
      val pencilID: Long = INSERT.INTO(prices).SET(prices.item_name := "pencil", prices.item_price := 299).key[Long]
      INSERT.INTO(sales).SET(sales.item_id := pencilID, sales.number_sold := 3).exec
      INSERT.INTO(sales).SET(sales.item_id := pencilID, sales.number_sold := 2).exec
      UPDATE(prices).SET(prices.item_price := 199).exec

      val totalRevenue = SELECT(SUM(sales.number_sold * prices.item_price)).FROM(sales).INNER_JOIN(prices).ON(sales.item_id === prices.item_id).only
      assert(totalRevenue === Some(199 * 5))
    }
  }

  describe("Alter table") {
    it("works") {
      ALTER.TABLE(Tab2).DROP.COLUMN(Tab2.col1).print.exec
      ALTER.TABLE(Tab2).DROP.COLUMN(Tab2.col2).print.exec
      ALTER.TABLE(Tab2).ADD.COLUMN(Tab2.col2, Tab2.col1).print.exec
      ALTER.TABLE(Tab2).MODIFY.COLUMN(Tab2.col2).print.exec
    }
  }
}
