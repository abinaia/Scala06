object Z1{
    case class Product(id: Int, name: String, quantity: Int, price: Double)

    var Inventory_1: Map[Int, Product] = Map(
        101 -> Product(101,"Pencil",10,25.99),
        102 -> Product(102,"Pen",20,30.0),
        103 -> Product(103,"Book",5,400.5)
    )

    var Inventory_2: Map[Int, Product] = Map(
        102 -> Product(102,"Pen",10,22.0),
        104 -> Product(102,"Eraser",30,15.0),
        103 -> Product(103,"Book",10,500.0)
    )

    def RetrieveName(list: Map[Int,Product]): Unit = {
        list.foreach { case (_, product) =>
            println(s"\t"+product.name)
        }
    }

    def TotalValues(list: Map[Int,Product]):Double = {
        var sum:Double = 0
        list.foreach { case (_, product) =>
            sum = sum + ((product.quantity) * (product.price))
        }
        return sum
    }

    def CheckEmpty(list: Map[Int,Product]):String= {
        if(list.isEmpty){
            return "Yes"
        }
        else{
            return "No"
        }
    }

    def Merge(list1: Map[Int, Product], list2: Map[Int, Product]): Map[Int, Product] = {
        (list1.keySet ++ list2.keySet).map { key =>
            val product1 = list1.getOrElse(key, Product(key, "", 0, 0.0))
            val product2 = list2.getOrElse(key, Product(key, "", 0, 0.0))

            key -> Product(
                key,
                if (product1.name.nonEmpty) product1.name else product2.name,
                product1.quantity + product2.quantity,
                math.max(product1.price, product2.price)
            )
        }.toMap
    }

    def exist(list: Map[Int, Product],Id: Int):Unit = {
        var found:Boolean = false
        list.foreach { case (_, product) =>
            if (product.id == Id){
                found = true
                println (s"Name = ${product.name} ")
                println (s"Name = ${product.quantity} ")
                println (s"Name = ${product.price} ")
            }
        }
        if(!found){
            println("Doesn't exist")
        }
    }

    
    def main(args: Array[String]): Unit= {
        println("Product Names in Inventory 1")
        RetrieveName(Inventory_1)
        println(s"\nTotal Value in Inventory 1 = ${TotalValues(Inventory_1)}")
        println(s"\nIs Inventory 1 Empty = ${CheckEmpty(Inventory_1)}")
        println("\nMerged Inventory:")
        val mergedInventory = Merge(Inventory_1, Inventory_2)
        mergedInventory.foreach { case (_, product) =>
            println(s"Product ID: ${product.id}, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}")
        }
        println()
        exist(Inventory_1,102)
    }
}