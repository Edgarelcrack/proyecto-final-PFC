/**

Taller 3 - Programación Funcional
Autores: <Estudiantes>
Profesor: Carlos A Delgado
 */
package Proyecto


import  Trie._
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Proyecto {
  case class Oraculo(predicado: Seq[Char] => Boolean)

  val alfabeto = Seq('a','c','g','t')

  def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {
    def generarRecursivo(cadenaParcial: Seq[Char], n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(cadenaParcial)
      else alfabeto.flatMap { caracter =>
        generarRecursivo(cadenaParcial :+ caracter, n - 1)
      }
    }
    val cerradura = generarRecursivo(Seq.empty[Char], n)
    cerradura.find { secuencia =>
      oraculo.predicado(secuencia)
    }.getOrElse(List.empty[Char])
  }

def reconstruirCadenasMejorado(n: Int, o: Oraculo): Seq[Char] = {
  val SC = alfabeto.filter(c => o.predicado(Seq(c)))

  def generarCombinaciones(cadenas: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
    if (n == 0) cadenas
    else {
      val nuevasCadenas = cadenas.flatMap(s1 => SC.map(c => s1 :+ c))
      generarCombinaciones(nuevasCadenas.filter(w => o.predicado(w)), n - 1)
    }
  }

  generarCombinaciones(Seq(Seq()), n).headOption.getOrElse(Seq())
}
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, alfabeto: Seq[Char]): Seq[Seq[Char]] = {
      if (k == 1) {
        alfabeto.map(Seq(_)).filter(subcadena => o.predicado(subcadena))
      } else {
        val scPrevias = generarCadenas(k-1, alfabeto)
        scPrevias.flatMap(sc => alfabeto.filter(c => o.predicado(sc :+ c)).map(sc :+ _))
      }
    }

    def combinarSubcadenas(subcadenas: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (k == n) {
        subcadenas.filter(subcadena => subcadena.length == n)
      } else {
        val nuevasSubcadenas = subcadenas.flatMap(sc => subcadenas.filter(c => o.predicado(sc ++ c)).map(sc ++ _))
        combinarSubcadenas(nuevasSubcadenas, k * 2)
      }
    }

    val subcadenasTamanio2 = generarCadenas(2, alfabeto)
    val resultado = combinarSubcadenas(subcadenasTamanio2, 2).find(subcadena => subcadena.length == n).getOrElse(Seq.empty[Char])

    resultado
  }
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k == 0) sc
      else {
        val sck = filtrar(sc, k, o)
        generarCadenas(k - 1, sck)
      }
    }
    def filtrar(sc: Seq[Seq[Char]], k: Int, oraculo: Oraculo): Seq[Seq[Char]] = {
      sc.flatMap { s1 =>
        alfabeto.flatMap { a =>
          val s2 = s1 :+ a
          if (oraculo.predicado(s2) && esFiltrable(s2, k, sc, oraculo)) Seq(s2)
          else Seq.empty
        }
      }
    }

    def esFiltrable(s: Seq[Char], k: Int, sc: Seq[Seq[Char]], oraculo: Oraculo): Boolean = {
      val subcadenas = (0 until k).map(i => s.drop(i).take(n))
      subcadenas.forall(w => oraculo.predicado(w) || sc.exists(_.startsWith(w)))
    }

    val sc = generarCadenas(n, Seq(Seq.empty[Char]))
    sc.reduce((s1, s2) => s1 ++ s2).take(n)
  }
  def reconstruirCadenasTurboAcelerado(n: Int, o: Oraculo): Seq[Char] = {
    def transformarCadena(s: Seq[Char], acc: Seq[Seq[Char]] = Seq.empty): Seq[Seq[Char]] = s match {
      case Nil => acc
      case _ => acc
    }

    val SC = alfabeto.map(Seq(_)).filter(o.predicado)

    def generarCombinaciones(cadenas: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (n <= 1) cadenas
      else {
        val cadenasArboles = cadenas.flatMap(lista => transformarCadena(lista, Seq(lista))).sortBy(_.length)
        val arbol = construirTrie(cadenasArboles)
        val filtrado = for {
          s1 <- cadenas
          s2 <- cadenas
          nuevaCadena = s1 ++ s2
          if nuevaCadena.length == 2 || nuevaCadena.sliding(nuevaCadena.length / 2).forall(par => buscar(arbol, par))
        } yield nuevaCadena

        generarCombinaciones(filtrado.filter(o.predicado), n / 2)
      }

    }

    generarCombinaciones(SC, n).head
  }


  def main(args: Array[String]): Unit = {
    val t = 8
    val secuancia = Seq('A', 'G', 'G', 'A','A', 'G', 'G', 'A')
    val oraculo: Oraculo = Oraculo(s => secuancia.containsSlice(s))
        println(reconstruirCadenaIngenuo(t, oraculo))

        println(reconstruirCadenasMejorado(t, oraculo))

        println(reconstruirCadenaTurbo(t, oraculo))

        println(reconstruirCadenaTurboMejorada(t, oraculo))

        println(reconstruirCadenasTurboAcelerado(t, oraculo))
  }
}