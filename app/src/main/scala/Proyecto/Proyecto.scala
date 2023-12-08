
package Proyecto
import common._
import org.scalameter.{Warmer, withWarmer}
import scala.collection.parallel.immutable.ParVector
import scala.util.Random



object proyecto {

 val alfabeto = Seq('a', 'c', 'g', 't')

 type Oraculo = Seq[Char] => Boolean
 abstract class Trie

 case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie

 case class Hoja(car: Char, marcada: Boolean) extends Trie

 def raiz(t: Trie): Char = {
  t match {
   case Nodo(c, _, _) => c
   case Hoja(c, _) => c
  }
 }

 def cabezas(t: Trie): Seq[Char] = {
  t match {
   case Nodo(_, _, hijos) => hijos.map(t => raiz(t))
   case Hoja(c, _) => Seq(c)
  }
 }

 def pertenece(s: Seq[Char], t: Trie): Boolean = {
  s match {
   case Seq() => true
   case head +: tail =>
    t match {
     case Nodo(c, _, hijos) if c == head =>
      hijos.exists(h => pertenece(tail, h))
     case _ => false
    }
  }
 }
/**
 def adicionar(s: Seq[Char], t: Trie): Trie = {
  s match {
   case Seq() => t // No se puede añadir una cadena vacía
   case c +: cs =>
    t match {
     case Nodo(r, m, hijos) =>
      val nuevoHijo = hijos.find(hijo => raiz(hijo) == c) match {
       case Some(hijoExistente) => adicionar(cs, hijoExistente)
       case None => Nodo(c, false, Seq()) // Crear un nuevo hijo
      }
      Nodo(r, m, hijos.filter(hijo => raiz(hijo) != c) :+ nuevoHijo)
     case _ => Nodo(c, false, Seq(adicionar(cs, Hoja(c, true))))
    }
  }
 }

 def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
  val trieVacio = Nodo(' ', false, Seq())
  ss.foldLeft(trieVacio)((t, s) => adicionar(s :+ '$', t)) // Agregar el delimitador '$'
 }

 def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
  true
 }

 def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
  true
 }

 def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
  true
 }

 def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
  true
 }

 def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
  true
 }

 def reconstruirCadenaIngenuoPar (umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
 true
 }
 def reconstruirCadenaMejoradoPar (umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
 true
 }
 def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
 true
 }
 def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
 true
 }
 def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
 true
 }
 */


 def main(args: Array[String]): Unit = {

  type Oraculo = Seq[Char] => Boolean

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
   def generarCombinaciones(n: Int, alfabeto: Seq[Char]): Seq[Seq[Char]] = {
    if (n == 0) Seq(Seq.empty)
    else {
     for {
      letra <- alfabeto
      combinacion <- generarCombinaciones(n - 1, alfabeto)
     } yield letra +: combinacion
    }
   }
   generarCombinaciones(n, alfabeto).find(o).getOrElse(Seq.empty)
  }
  val oraculoEjemplo: Oraculo = {
   case Seq('a','g','g','a') => true
   case _ => false
  }
  val cadenaReconstruida = reconstruirCadenaIngenuo(4, oraculoEjemplo)
  println(cadenaReconstruida)
 }
}