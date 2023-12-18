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

object Proyecto {
  case class Oraculo(predicado: Seq[Char] => Boolean)

  val alfabeto = Seq('a','c','g','t')

  def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {

    //Cadena parcial -> almacena las combinaciones
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
    cerradura.find { secuencia =>
      oraculo.predicado(secuencia)
    }.getOrElse(List.empty[Char])
  }

def reconstruirCadenasMejorado(n: Int, o: Oraculo): Seq[Char] = {
  //Se sacan las cadenas que son subcadenas del original con el alfabeto
  val SC = alfabeto.filter(c => o.predicado(Seq(c)))

  def generarCombinaciones(cadenas: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
    //La variable cadenas almacena las combinaciones que se estan generando
    if (n == 0) cadenas
    else {
      // A cada elemento del alfabeto se combinara con el elemento
      val nuevasCadenas = cadenas.flatMap(s1 => SC.map(c => s1 :+ c))
      //Luego de hacer las combinaciones se vuelve a filtrar y se envia las cadenas que si son subcadenas
      generarCombinaciones(nuevasCadenas.filter(w => o.predicado(w)), n - 1)
    }
  }

  generarCombinaciones(Seq(Seq()), n).headOption.getOrElse(Seq())
}
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {

    def generarCadenas(k: Int, alfabeto: Seq[Char]): Seq[Seq[Char]] = {
      //Con el mapeo se trata a cada elemento del alfabeto como una subcadena
      //Con el filter se tomaran solo las que son subcadenas de la cadena que se quiere encontrar
      if (k == 1) {
        alfabeto.map(Seq(_)).filter(subcadena => o.predicado(subcadena))
      } else {
        //almacena lo que se esta generando en la condicion anterior
        val scPrevias = generarCadenas(k-1, alfabeto)

        //A cada secuencia en scPrevias se le concatena cada caracter del alfabeto que cumple con el predicado
        //Se analiza si esa concatenacion cumple con el predicado y si asi es entonces se le concatenara ese caracter
        //Del alfabeto a la secuencia que se encuentra en scPrevias
        scPrevias.flatMap(sc => alfabeto.filter(c => o.predicado(sc :+ c)).map(sc :+ _))
      }
    }

    def combinarSubcadenas(subcadenas: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (k == n) {
        //Se saca la cadena que estabamos buscando
        subcadenas.filter(subcadena => subcadena.length == n)
      } else {

        //Caso contrario se empiezan hacer combinaciones en potencias de 2
        //Esta funcion solo sera para secuencias de 2**n
        //Como no es del tamaño se empiezan hacer combinaciones de 2 en 2
        //De esas solo se dejaran las que cumplen con el predicado
        val nuevasSubcadenas = subcadenas.flatMap(sc => subcadenas.filter(c => o.predicado(sc ++ c)).map(sc ++ _))
        //Se hace el llamado recursivo como se combino con si misma entonces por eso se envia k * 2
        //Se envie y se hace lo mismo hasta que cumpla con el tamaño de la cadena que se esta buscando
        combinarSubcadenas(nuevasSubcadenas, k * 2)
      }
    }

    //Casos base ya que el algoritmo solo funciona con potencias de 2
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
        // Ejecución paralela
        val resultadosParalelos = alfabeto.map { caracter =>
          task {
            generarRecursivoParalelo(cadenaParcial :+ caracter, n - 1)
          }
        }
        val resultadosSeq = resultadosParalelos.map(_.join())
        resultadosSeq.flatMap(identity)
      } else {
        // Ejecución secuencial
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


  def reconstruirCadenaMejoradoParalelo(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val SC = alfabeto.map(Seq(_)).filter(w => o.predicado(w))

    def generarCombinacionesParalelo(cadenas: Seq[Seq[Char]], n: Int): Seq[Seq[Char]] = {
      if (n == 0) cadenas
      else if (umbral > n) {
        // Ejecución paralela
        val nuevasCadenasParalelas = cadenas.flatMap { s1 =>
          val tareas = SC.map { s2 =>
            task {
              s1 ++ s2
            }
          }
          tareas.map(_.join())
        }
        generarCombinacionesParalelo(nuevasCadenasParalelas.filter(w => o.predicado(w)), n - 1)
      } else {
        // Ejecución secuencial
        val nuevasCadenas = cadenas.flatMap(s1 => SC.map(s2 => s1 ++ s2))
        generarCombinacionesParalelo(nuevasCadenas.filter(w => o.predicado(w)), n - 1)
      }
    }

    generarCombinacionesParalelo(SC, n).headOption.getOrElse(List.empty[Char])
  }

  def reconstruirCadenaTurboParalelo(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarCadenasParalelo(k: Int, alfabeto: Seq[Char]): Seq[Seq[Char]] = {
      if (k == 1) {
        // Ejecución secuencial
        alfabeto.map(Seq(_)).filter(subcadena => o.predicado(subcadena))
      } else if (umbral > n) {
        // Ejecución paralela
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
        // Ejecución secuencial
        val nuevasSubcadenas = subcadenas.flatMap(sc => subcadenas.filter(c => o.predicado(sc ++ c)).map(sc ++ _))
        combinarSubcadenasParalelo(nuevasSubcadenas, k * 2)
      }
    }

    val subcadenasTamanio2 = generarCadenasParalelo(2, alfabeto)
    val resultado = combinarSubcadenasParalelo(subcadenasTamanio2, 2).find(subcadena => subcadena.length == n).getOrElse(Seq.empty[Char])

    resultado
  }


  def main(args: Array[String]): Unit = {
    val t = 8
    val secuancia = Seq('a', 'g', 'g', 'a','a', 'g', 'g', 'a')
    val oraculo: Oraculo = Oraculo(s => secuancia.containsSlice(s))
        println(reconstruirCadenaIngenuo(t, oraculo))

        println(reconstruirCadenasMejorado(t, oraculo))

        println(reconstruirCadenaTurbo(t, oraculo))

        println(reconstruirCadenaTurboMejorada(t, oraculo))

        println(reconstruirCadenasTurboAcelerado(t, oraculo))
  }
}