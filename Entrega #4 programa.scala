import scala.io.StdIn.readLine

// Modelo de producto
case class Producto(id: Int, nombre: String, categoria: String, precio: Double, stock: Int)

object TiendaPro {

  // Inventario inicial
  var inventario: List[Producto] = List(
    Producto(1, "Muñeca", "Juguetes", 15, 10),
    Producto(2, "Pan", "Comida", 2, 50),
    Producto(3, "Detergente", "Limpieza", 5, 20),
    Producto(4, "Camisa", "Ropa", 20, 15),
    Producto(5, "Zapatos", "Calzado", 35, 10),
    Producto(6, "Martillo", "Herramientas", 12, 8)
  )

  // Carrito: ID -> cantidad
  var carrito: Map[Int, Int] = Map()

  var siguienteId: Int = 7

  def main(args: Array[String]): Unit = {
    var opcion = ""

    while (opcion != "7") {
      println("\n===== TIENDA PRO =====")
      println("1. Ver productos")
      println("2. Comprar producto")
      println("3. Ver carrito")
      println("4. Buscar producto")
      println("5. Agregar producto")
      println("6. Finalizar compra")
      println("7. Salir")

      opcion = readLine("Opción: ")

      opcion match {
        case "1" => mostrarProductos()
        case "2" => comprarProducto()
        case "3" => verCarrito()
        case "4" => buscarProducto()
        case "5" => agregarProducto()
        case "6" => finalizarCompra()
        case "7" => println("Gracias por usar la tienda")
        case _   => println("Opción inválida")
      }
    }
  }

  def mostrarProductos(): Unit = {
    println("\n--- Productos ---")
    inventario.foreach { p =>
      println(s"${p.id}. ${p.nombre} (${p.categoria}) - $$${p.precio} (Stock: ${p.stock})")
    }
  }

  def comprarProducto(): Unit = {
    mostrarProductos()

    val id = readLine("ID del producto: ").toInt
    val cantidad = readLine("Cantidad: ").toInt

    val productoOpt = inventario.find(_.id == id)

    productoOpt match {
      case Some(p) =>
        if (p.stock >= cantidad) {
          carrito += (id -> (carrito.getOrElse(id, 0) + cantidad))

          inventario = inventario.map { prod =>
            if (prod.id == id) prod.copy(stock = prod.stock - cantidad)
            else prod
          }

          println(s"Agregado al carrito: ${p.nombre}")
        } else {
          println("Stock insuficiente")
        }

      case None =>
        println("Producto no encontrado")
    }
  }

  def verCarrito(): Unit = {
    println("\n--- Carrito ---")

    if (carrito.isEmpty) {
      println("Carrito vacío")
    } else {
      carrito.foreach { case (id, cantidad) =>
        val producto = inventario.find(_.id == id)
        producto.foreach { p =>
          println(s"${p.nombre} x$cantidad = $$${p.precio * cantidad}")
        }
      }
    }
  }

  def buscarProducto(): Unit = {
    val texto = readLine("Buscar: ").toLowerCase

    val resultados = inventario.filter(_.nombre.toLowerCase.contains(texto))

    resultados.foreach { p =>
      println(s"${p.id}. ${p.nombre} - $$${p.precio}")
    }
  }

  def agregarProducto(): Unit = {
    val nombre = readLine("Nombre: ")
    val categoria = readLine("Categoría: ")
    val precio = readLine("Precio: ").toDouble
    val stock = readLine("Stock: ").toInt

    val nuevo = Producto(siguienteId, nombre, categoria, precio, stock)
    inventario = inventario :+ nuevo
    siguienteId += 1

    println("Producto agregado")
  }

  def finalizarCompra(): Unit = {
    println("\n--- Total ---")

    var total = 0.0

    carrito.foreach { case (id, cantidad) =>
      val producto = inventario.find(_.id == id)
      producto.foreach { p =>
        total += p.precio * cantidad
      }
    }

    println(f"Total a pagar: $$${total}%.2f")

    carrito = Map() // vaciar carrito
  }
}