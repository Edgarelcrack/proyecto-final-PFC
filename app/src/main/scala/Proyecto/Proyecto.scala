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

  val alfabeto = Seq('A','C','G','T')
  type Oraculo = Seq[Char] => Boolean



  def main(args: Array[String]): Unit = {
    val secuancia = Seq('A','G','G','A')
    val oraculo: Oraculo = (s: Seq[Char]) => {
      secuancia.containsSlice(s)
    }
    println(reconstruirCadenaIngenuo(4, oraculo))
    println(reconstruirCadenaMejorado(4,oraculo))
    println(reconstruirCadenaTurbo(4,oraculo))

  }

  def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {
    def generarRecursivo(cadenaParcial: Seq[Char], n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(cadenaParcial)
      else alfabeto.flatMap { caracter =>
        generarRecursivo(cadenaParcial :+ caracter, n - 1)
      }
    }
    val cerradura = generarRecursivo(Seq.empty[Char], n)
    cerradura.find { secuencia =>
      oraculo(secuencia)
    }.getOrElse(List.empty[Char])
  }


  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > n) sc
      else { val sck = sc.flatMap(s => alfabeto.map(a => s :+ a)).filter(o);
        generarCadenas(k + 1, sck);
      }
    }

    val sc = generarCadenas(1, Seq(Seq()));
    sc.find(_.length == n).getOrElse(Seq());
  }

  def reconstruirCadenaMejorado2(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > n) sc
      else { val sck = sc.flatMap(s => alfabeto.map(a => s :+ a)).filter(o)
        generarCadenas(k + 1, sck)
      }
    }
    val sc = generarCadenas(1, Seq(Seq.empty[Char]))
    sc.reduce((s1, s2) => s1 ++ s2).take(n)
  }


}