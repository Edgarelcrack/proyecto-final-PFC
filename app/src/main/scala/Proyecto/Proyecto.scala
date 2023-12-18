/**

Taller 3 - Programación Funcional
Autores: <Edgar Andres Vargas Garcia-2259690 - Juan Pablo Escovar Viveros-2259519 - Nicolas Mauricio Rojas-2259460 >
Profesor: Carlos A Delgado
 */
package Proyecto


import  Trie._
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import common._
import org.scalameter.Quantity

object Proyecto {
  case class Oraculo(predicado: Seq[Char] => Boolean)

  val alfabeto: Seq[Char] = Seq('a','c','g','t')

  def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {
    def generarRecursivo(cadenaParcial: Seq[Char], n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(cadenaParcial)
      //Para cada caracter del alfabeto un llamado recursivo con la concatenacion
      else alfabeto.flatMap { caracter =>
        generarRecursivo(cadenaParcial :+ caracter, n - 1)
      }
    }
    //Cerradura es quien tiene todas las combinaciones posibles
    val cerradura = generarRecursivo(Seq.empty[Char], n)
    //Funcion find que encontrara la cadena que corresponda al oraculo
    cerradura.find { secuencia =>oraculo.predicado(secuencia)
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

  def reconstruirCadenaIngenuoParalela(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarRecursivoParalelo(cadenaParcial: Seq[Char], n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(cadenaParcial)
      else if (umbral > n) {
        val resultadosParalelos = alfabeto.map { caracter =>
          task {
            generarRecursivoParalelo(cadenaParcial :+ caracter, n - 1)
          }
        }
        val resultadosSeq = resultadosParalelos.map(_.join())
        resultadosSeq.flatMap(identity)
      } else {
        alfabeto.flatMap { caracter =>
          generarRecursivoParalelo(cadenaParcial :+ caracter, n - 1)
        }
      }
    }
    val cerradura = generarRecursivoParalelo(Seq.empty[Char], n)
    cerradura.find { secuencia =>
      o.predicado(secuencia)
    }.getOrElse(List.empty[Char])
  }

  def reconstruirCadenasMejoradoParalelo(n: Int, o: Oraculo): Seq[Char] = {
    val SC = alfabeto.map(Seq(_)).filter(w => o.predicado(w));

    def generarCombinaciones(cadenas: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (n == 1) cadenas
      else {
        val size = cadenas.length / 2;
        val (parte1, parte2) = parallel(
          cadenas.take(size).flatMap(s1 => SC.map(s2 => s1 ++ s2)),
          cadenas.drop(size).flatMap(s1 => SC.map(s2 => s1 ++ s2))
        );

        generarCombinaciones((parte1 ++ parte2).filter(w => o.predicado(w)), n - 1);
      }
    }

    generarCombinaciones(SC, n).head;
  }

  def reconstruirCadenaTurboParalelo(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenasParalelo(k: Int, alfabeto: Seq[Char]): Seq[Seq[Char]] = {
      if (k == 1) {
        alfabeto.map(Seq(_)).filter(subcadena => o.predicado(subcadena))
      } else if (umbral > n) {
        val scPreviasParalelas = generarCadenasParalelo(k - 1, alfabeto)
        val nuevasCadenasParalelas = scPreviasParalelas.flatMap { sc =>
          val tareas = alfabeto.filter(c => o.predicado(sc :+ c)).map { c =>
            task {
              sc :+ c
            }
          }
          tareas.map(_.join())
        }
        nuevasCadenasParalelas
      } else {
        // Ejecución secuencial
        val scPrevias = generarCadenasParalelo(k - 1, alfabeto)
        val nuevasCadenas = scPrevias.flatMap(sc => alfabeto.filter(c => o.predicado(sc :+ c)).map(sc :+ _))
        nuevasCadenas
      }
    }

    def combinarSubcadenasParalelo(subcadenas: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (k == n) {
        // Ejecución secuencial
        subcadenas.filter(subcadena => subcadena.length == n)
      } else if (umbral > n) {
        // Ejecución paralela
        val nuevasSubcadenasParalelas = subcadenas.flatMap { sc =>
          val tareas = subcadenas.filter(c => o.predicado(sc ++ c)).map { c =>
            task {
              sc ++ c
            }
          }
          tareas.map(_.join())
        }
        combinarSubcadenasParalelo(nuevasSubcadenasParalelas, k * 2)
      } else {

        val nuevasSubcadenas = subcadenas.flatMap(sc => subcadenas.filter(c => o.predicado(sc ++ c)).map(sc ++ _))
        combinarSubcadenasParalelo(nuevasSubcadenas, k * 2)
      }
    }

    val subcadenasTamanio2 = generarCadenasParalelo(2, alfabeto)
    val resultado = combinarSubcadenasParalelo(subcadenasTamanio2, 2).find(subcadena => subcadena.length == n).getOrElse(Seq.empty[Char])

    resultado
  }

  def reconstruirCadenaTurboMejoradaParalela(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenas(k: Int, sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k == 0) sc
      else {
        val sck = parallel(filtrar(sc, k, o), Seq.empty[Seq[Char]])._1
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


  def compararAlgoritmos(Funcion1: (Int, Oraculo) => Seq[Char], Funcion2: (Int, Oraculo) => Seq[Char])(n: Int, o: Oraculo): (Double, Double, Double) = {
    val timeF1 = withWarmer(new Warmer.Default) measure {
      Funcion1(n, o);
    }
    val timeF2 = withWarmer(new Warmer.Default) measure {
      Funcion2(n, o);
    }

    val promedio = timeF1.value / timeF2.value;
    (timeF1.value, timeF2.value, promedio);
  }
  def main(args: Array[String]): Unit = {

    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 10000).toArray
      }
    );

    val t = 8
    val secuencia = Seq('c','c','a','a','c','c','a','a')

    val oraculo: Oraculo = Oraculo(s => secuencia.containsSlice(s))


    println("Ingenuas")
        println(reconstruirCadenaIngenuo(t, oraculo))
        println(reconstruirCadenaIngenuoParalela(t+1)(t, oraculo))
        println(compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoParalela(8))(t, oraculo))
    println("mejorada")
        println(reconstruirCadenasMejorado(t, oraculo))
        println(reconstruirCadenasMejoradoParalelo(t, oraculo))
        println(compararAlgoritmos(reconstruirCadenasMejorado,reconstruirCadenasMejoradoParalelo)(t,oraculo))
    println("Turbo")
        println(reconstruirCadenaTurbo(t, oraculo))
        println(reconstruirCadenaTurboParalelo(t+1)(t, oraculo))
        println(compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboParalelo(8))(t,oraculo))
    println("Turbomejorada")
        println(reconstruirCadenaTurboMejorada(t, oraculo))
        println(reconstruirCadenaTurboMejoradaParalela(t, oraculo))
        println(compararAlgoritmos(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboMejoradaParalela)(t,oraculo))
    println("acelerada")
        println(reconstruirCadenasTurboAcelerado(t, oraculo))


  }
}