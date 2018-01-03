package edu.agh.automata.utils

import org.scalacheck.Gen

object Generators {

  def niceName(min: Int, max: Int): Gen[String] = {
    // letter frequency based on wikipedia for english alphabet
    val consonant = Seq(
      15 -> 'b',
      28 -> 'c',
      43 -> 'd',
      22 -> 'f',
      20 -> 'g',
      61 -> 'h',
      2 -> 'j',
      8 -> 'k',
      40 -> 'l',
      24 -> 'm',
      67 -> 'n',
      19 -> 'p',
      1 -> 'q',
      60 -> 'r',
      63 -> 's',
      90 -> 't',
      10 -> 'v',
      24 -> 'w',
      2 -> 'x',
      20 -> 'y',
      1 -> 'z',
    ) map Gen.freqTuple
    val vowel = Seq(
      82 -> 'a',
      127 -> 'e',
      70 -> 'i',
      75 -> 'o',
      28 -> 'u',
    ) map Gen.freqTuple

    val vowelGen = Gen.frequency(vowel: _*)
    val consonantGen = Gen.frequency(consonant: _*)

    for {
      length <- Gen.choose(min, max)
      halfLen = length / 2
      vowels <- Gen.listOfN(halfLen, vowelGen)
      consonants <- Gen.listOfN(halfLen, consonantGen)
      ending <- Gen.listOfN(length % 2, consonantGen)
    } yield ((consonants, vowels).zipped.flatMap(List(_, _)) ++ ending).mkString
  }

  def randomName: String = niceName(6, 10).sample.get

}
