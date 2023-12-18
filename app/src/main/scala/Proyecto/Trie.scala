package object Trie {
  abstract class Trie;

  case class Nodo(car: Char, marcado: Boolean, hijos: Seq[Trie]) extends Trie;

  case class Hoja(car: Char, marcado: Boolean) extends Trie;

  def raiz(t: Trie): Char = t match {
    case Nodo(car, marcado, _) => car;
    case Hoja(car, marcado) => car;
  }

  def cabezas(t: Trie): Seq[Char] = t match {
    case Nodo(_, _, hijos) => hijos.map(raiz);
    case Hoja(car, marcado) => Seq[Char](car);
  }

  def convertirNodosSinHijosEnHojas(t: Trie): Trie = t match {
    case Nodo(car, marcado, hijos) =>
      val nuevosHijos = hijos.map(convertirNodosSinHijosEnHojas)
      if (nuevosHijos.isEmpty) Hoja(car, marcado)
      else Nodo(car, marcado, nuevosHijos)
    case hoja@Hoja(_, _) => hoja
  }

  def buscar(raiz: Trie, secuencia: Seq[Char]): Boolean = {
    secuencia match {
      case Nil => raiz match {
        case Nodo(_, marcado, _) => marcado
        case Hoja(_, _) => true
      }
      case c :: cs => raiz match {
        case Hoja(_, _) => false
        case Nodo(_, _, hijos) =>
          if (cabezas(Nodo(' ', false, hijos)).contains(c)) {
            hijos.find {
              case Nodo(hijoCar, _, _) => hijoCar == c
              case Hoja(hijoCar, _) => hijoCar == c
            } match {
              case Some(nodoHijo) => buscar(nodoHijo, cs)
              case None => false
            }
          } else {
            false
          }
      }
    }
  }

  def insertarEnTrie(raiz: Trie, palabra: Seq[Char]): Trie = {
    palabra match {
      case Nil => raiz
      case c :: cs => raiz match {
        case Nodo(car, marcado, hijos) =>
          val hijo = hijos.find {
            case Nodo(hijoCar, _, _) => hijoCar == c
            case _ => false
          }
          hijo match {
            case Some(nodoHijo: Nodo) => Nodo(car, marcado, hijos.filter(_ != nodoHijo) :+ insertarEnTrie(nodoHijo, cs))
            case None => Nodo(car, marcado, hijos :+ insertarEnTrie(Nodo(c, cs.isEmpty, Seq()), cs))
          }
      }
    }
  }
  def construirTrie(palabras: Seq[Seq[Char]]): Trie = {
    convertirNodosSinHijosEnHojas(palabras.foldLeft(Nodo(' ', false, Seq()): Trie) { (raiz, palabra) =>
      insertarEnTrie(raiz, palabra)
    });
  }
}
