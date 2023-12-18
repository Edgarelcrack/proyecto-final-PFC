/**
 * Plantilla para pruebas
 * Autores: <Edgar Andres Vargas Garcia-2259690 - Juan Pablo Escovar Viveros-2259519 >
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package Proyecto


import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite {

  test("Ingenua 1") {
    val secuencia = Seq('a', 'a', 'c', 'c', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s)) // Crea el oráculo y lo asigna a objOraculo
    val n = 6
    assert(Proyecto.reconstruirCadenaIngenuo(n, objOraculo) == List('a', 'a', 'c', 'c', 'g', 'g')) // Usa objOraculo aquí
  }


}
