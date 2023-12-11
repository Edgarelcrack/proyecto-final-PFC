/**

Taller 3 - Programación Funcional
Autores: <Estudiantes>
Profesor: Carlos A Delgado
 */
package Proyecto

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Proyecto{

 def main(args: Array[String]): Unit = {
  val x = List("C", "T", "A", "A", "C")
  val w = PRCingenuo(x, x.length, oraculo)
  println(w)
 }


 def oraculo(sub: String, cad: String): Boolean = {
  cad.contains(sub)
 }

 def PRCingenuo(L: List[String], n: Int, f: (String, String) => Boolean): List[String] = {
  require(n > 0)

  def cerraduraIngenua(l: List[String], n: Int): List[String] = {
   def generarCerraduraKleene(c: List[String], m: Int): List[String] = {
    if (m > n) List.empty
    else c ::: generarCerraduraKleene(c.flatMap(c => l.map(c + _)), m + 1)
   }
   generarCerraduraKleene(l, n)
  }

  val s = cerraduraIngenua(L, n)
  val g = s.takeWhile(f(_, (L.toString())))
  g
 }
}