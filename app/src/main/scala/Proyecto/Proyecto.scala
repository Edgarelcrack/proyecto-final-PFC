/**

Taller 3 - Programación Funcional
Autores: <Estudiantes>
Profesor: Carlos A Delgado
 */
package Proyecto



import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object proyecto {
  case class Oraculo(predicado: Seq[Char] => Boolean)

  val alfabeto = Seq('A','C','G','T')

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

  def reconstruirCadenaMejorado_(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > n) sc
      else { val sck = sc.flatMap(s => alfabeto.map(a => s :+ a)).filter(o.predicado);
        generarCadenas(k + 1, sck);
      }
    }

    val sc = generarCadenas(1, Seq(Seq()));
    sc.find(_.length == n).getOrElse(Seq());
  }
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > n) sc
      else {
        val sck = sc.flatMap { s =>
          alfabeto.map(a => s :+ a).filter(o.predicado)
        }
        generarCadenas(k + 1, sck)
      }
    }

    def esSubcadenaValida(s: Seq[Char]): Boolean = {
      (1 until s.length).forall { i =>
        o.predicado(s.take(i)) && o.predicado(s.drop(i))
      }
    }

    val sc = generarCadenas(1, Seq(Seq()))
    sc.find(esSubcadenaValida).getOrElse(Seq())
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
    require(n > 0 && (n & (n - 1)) == 0)
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

  def main(args: Array[String]): Unit = {
    val t = 8
    val secuancia = Seq('A', 'G', 'G', 'A', 'A', 'G', 'G', 'A')
    val oraculo: Oraculo = Oraculo(s => secuancia.containsSlice(s))


    println(reconstruirCadenaIngenuo(t, oraculo))


        println(reconstruirCadenaMejorado(t, oraculo))

        println(reconstruirCadenaTurbo(t, oraculo))

        println(reconstruirCadenaTurboMejorada(t, oraculo))

  }
}