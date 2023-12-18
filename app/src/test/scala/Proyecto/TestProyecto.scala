/**
 * Plantilla para pruebas
 * Autores: <Edgar Andres Vargas Garcia-2259690 - Juan Pablo Escovar Viveros-2259519 - Nicolas Mauricio Rojas-2259460 >
 * @version 1.0
 * @note 22 de Noviembre de 2023
 */
package Proyecto


import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestProyecto extends AnyFunSuite {

  test("Ingenua 1") {
    val secuencia = Seq('a', 'a', 'c', 'c', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s)) // Crea el oráculo y lo asigna a objOraculo
    val n = 6
    assert(Proyecto.reconstruirCadenaIngenuo(n, objOraculo) == List('a', 'a', 'c', 'c', 'g', 'g')) // Usa objOraculo aquí
  }

  test("Ingenua 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    assert(Proyecto.reconstruirCadenaIngenuo(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Ingenua 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenaIngenuo(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g'))
  }

  test("Ingenua 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenaIngenuo(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't'))
  }

  test("Mejorado 1") {
    val secuencia = Seq('t', 'a', 'a', 'a', 'a', 'g', 'c', 'g', 'c', 'c', 'a', 'a')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 12
    assert(Proyecto.reconstruirCadenasMejorado(n, objOraculo) == List('t', 'a', 'a', 'a', 'a', 'g', 'c', 'g', 'c', 'c', 'a', 'a'))
  }
  test("Mejorado 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 12
    assert(Proyecto.reconstruirCadenasMejorado(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Mejorado 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 12
    assert(Proyecto.reconstruirCadenasMejorado(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c'))
  }

  test("Mejorado 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 14
    assert(Proyecto.reconstruirCadenasMejorado(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g'))
  }

  test("Turbo 1") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    assert(Proyecto.reconstruirCadenaTurbo(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }

  test("Turbo 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 16
    assert(Proyecto.reconstruirCadenaTurbo(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Turbo 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 32
    assert(Proyecto.reconstruirCadenaTurbo(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't'))
  }

  test("Turbo 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 64
    assert(Proyecto.reconstruirCadenaTurbo(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }

  test("Turbo mejorada 1") {
    val secuencia = Seq('a', 'a', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 4
    assert(Proyecto.reconstruirCadenaTurboMejorada(n, objOraculo) == List('a', 'a', 'c', 'c'))
  }

  test("Turbo mejorada 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    assert(Proyecto.reconstruirCadenaTurboMejorada(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Turbo mejorada 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 16
    assert(Proyecto.reconstruirCadenaTurboMejorada(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't'))
  }

  test("Turbo mejorada 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 32
    assert(Proyecto.reconstruirCadenaTurboMejorada(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }

  test("Turbo acelerada 1") {
    val secuencia = Seq('a', 'a', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 4
    assert(Proyecto.reconstruirCadenasTurboAcelerado(n, objOraculo) == List('a', 'a', 'c', 'c'))
  }

  test("Turbo acelerada 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    assert(Proyecto.reconstruirCadenasTurboAcelerado(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Turbo acelerada 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 16
    assert(Proyecto.reconstruirCadenasTurboAcelerado(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't'))
  }

  test("Turbo acelerada 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 32
    assert(Proyecto.reconstruirCadenasTurboAcelerado(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }

  test("Ingenua Paralela 1") {
    val secuencia = Seq('a', 'a', 'c', 'c', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 6
    val umbral = 0
    assert(Proyecto.reconstruirCadenaIngenuoParalela(umbral)(n, objOraculo) == List('a', 'a', 'c', 'c', 'g', 'g'))
  }

  test("Ingenua Paralela 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    assert(Proyecto.reconstruirCadenaIngenuoParalela(2)(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Ingenua Paralela 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenaIngenuoParalela(4)(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g'))
  }

  test("Ingenua Paralela 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenaIngenuoParalela(6)(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't'))
  }

  test("Mejorado Paralelo 1") {
    val secuencia = Seq('t', 'a', 'a', 'a', 'a', 'g', 'c', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenasMejoradoParalelo(n, objOraculo) == List('t', 'a', 'a', 'a', 'a', 'g', 'c', 'g', 'c', 'c'))
  }
  test("Mejorado Paralelo 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenasMejoradoParalelo(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c'))
  }

  test("Mejorado Paralelo 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenasMejoradoParalelo(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g'))
  }

  test("Mejorado Paralelo 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 10
    assert(Proyecto.reconstruirCadenasMejoradoParalelo(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't'))
  }

  test("Turbo Paralelo 1") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    assert(Proyecto.reconstruirCadenaTurboParalelo(4)(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }

  test("Turbo Paralelo 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 16
    assert(Proyecto.reconstruirCadenaTurboParalelo(8)(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Turbo Paralelo 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 32
    assert(Proyecto.reconstruirCadenaTurboParalelo(16)(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't'))
  }

  test("Turbo Paralelo 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 64
    assert(Proyecto.reconstruirCadenaTurboParalelo(32)(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }
  test("Turbo mejorada Paralelo 1") {
    val secuencia = Seq('a', 'a', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 4
    val u = 4
    assert(Proyecto.reconstruirCadenaTurboMejoradaParalela(u)(n, objOraculo) == List('a', 'a', 'c', 'c'))
  }

  test("Turbo mejorada Paralelo 2") {
    val secuencia = Seq('a', 'c', 'g', 't', 'a', 'c', 'g', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 8
    val u = 4
    assert(Proyecto.reconstruirCadenaTurboMejoradaParalela(u)(n, objOraculo) == List('a', 'c', 'g', 't', 'a', 'c', 'g', 't'))
  }

  test("Turbo mejorada Paralelo 3") {
    val secuencia = Seq('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 16
    val u = 4
    assert(Proyecto.reconstruirCadenaTurboMejoradaParalela(u)(n, objOraculo) == List('g', 'g', 'c', 'c', 'a', 'a', 't', 't', 'g', 'g', 'c', 'c', 'a', 'a', 't', 't'))
  }

  test("Turbo mejorada Paralelo 4") {
    val secuencia = Seq('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c')
    val objOraculo = Proyecto.Oraculo(s => secuencia.containsSlice(s))
    val n = 32
    val u = 4
    assert(Proyecto.reconstruirCadenaTurboMejoradaParalela(u)(n, objOraculo) == List('t', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c', 't', 't', 'a', 'a', 'g', 'g', 'c', 'c'))
  }


}
