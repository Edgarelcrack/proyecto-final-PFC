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
    val f = generarCerraduraKleene(alfabeto,4)
    val oraculo: Oraculo = (secuencia: Seq[Char]) => secuencia.mkString == "ACGT"
    val resultado = reconstruirCadenaIngenuo(4, oraculo)
    println(f)
  }

  def generarCerraduraKleene(alfabeto: Seq[Char], longitud: Int): Seq[String] = {
    def generarRecursivo(cadenaParcial: String, n: Int): Seq[String] = {
      if (n == 0) Seq(cadenaParcial)
      else {
        val subcadenas = alfabeto.flatMap { caracter =>
          generarRecursivo(cadenaParcial + caracter, n - 1)
        }
        Seq(cadenaParcial) ++ subcadenas
      }
    }

    (1 to longitud).flatMap { n =>
      generarRecursivo("", n)
    }
  }
  def reconstruirCadenaIngenuo(n:Int, oraculo: Oraculo): Option[String] = {
    val cerradura = generarCerraduraKleene(alfabeto, n)
    cerradura.find { secuencia =>
      oraculo(secuencia.toSeq)
    }
  }
}