package exploration.search

import elevate.rise.Rise
import shine.C.Program
import util.{Execute2, gen}

object executeC {
  val N = 1024

  def apply(riseProgram:Rise): Double = {

    //generate executable program (including host code)
    val code = genExecutableCode(riseProgram)

//    println("code: " + code )
    //compile and execute program
    val performanceValue = compileAndExecute(code)

    performanceValue
  }

  def prepareInput(riseProgram:Program):(String,String,String) ={

    val arrayTwo = "..[.]..[.]f32".r
    val arrayOne = "..[.]f32".r
    val elemOne = "f32".r

    var codeBeg = s"""
        const int N = ${N};
        """

    var codeEnd =
      s"""
       //free memory"""

    var call = s"""${riseProgram.function.name}(output"""

    codeBeg +=
      s"""
        //inputs""".stripMargin
    riseProgram.inputParams.foreach(elem => {
      if (elem.`type`.dataType.toString.equals("int")) {
        codeBeg +=
          s"""
        const int ${elem.name} = N; """
        call += s""", ${elem.name}"""
      } else if (arrayTwo.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float* ${elem.name} = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${elem.name}[i] = i;
        }
        """
        codeEnd +=
          s"""
        free(${elem.name});"""
        call += s""", ${elem.name}"""
      } else if (arrayOne.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float* ${elem.name} = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${elem.name}[i] = i;
        }
        """
        codeEnd +=
          s"""
        free(${elem.name});"""
        call += s""", ${elem.name}"""
      } else if (elemOne.findFirstIn(elem.`type`.dataType.toString).size > 0) {
        codeBeg +=
          s"""
        float ${elem.name} = 5;
        """
        call += s""", ${elem.name}"""
      }
    })

    codeBeg += s"""

        //output"""

      if(riseProgram.outputParam.`type`.dataType.toString.equals("int")) {
        codeBeg +=
          s"""
        const int ${riseProgram.outputParam.name} = N; """
      } else if (arrayTwo.findFirstIn(riseProgram.outputParam.`type`.dataType.toString).size > 0) {
        codeBeg += s"""
        float* ${riseProgram.outputParam.name} = (float*) malloc(sizeof(float)*N*N);
        for (int i = 0; i < N*N; i++) {
          ${riseProgram.outputParam.name}[i] = 0;
        }
        """
        codeEnd += s"""
        free(${riseProgram.outputParam.name});"""
      } else if (arrayOne.findFirstIn(riseProgram.outputParam.`type`.dataType.toString).size > 0) {
        codeBeg += s"""
        float* ${riseProgram.outputParam.name} = (float*) malloc(sizeof(float)*N);
        for (int i = 0; i < N; i++) {
          ${riseProgram.outputParam.name}[i] = 0;
        }
        """
        codeEnd += s"""
        free(${riseProgram.outputParam.name});"""

      } else if (elemOne.findFirstIn(riseProgram.outputParam.`type`.dataType.toString).size > 0) {
        codeBeg += s"""
        float ${riseProgram.outputParam.name} = N;
        """
      }

      call += s""");"""

      (codeBeg,codeEnd, call)
    }

    def genExecutableCode(riseProgram:Rise):String = {

      val p = gen.CProgram(riseProgram)

      val preparation = prepareInput(p)

    val testCode = s"""
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
${p.code}

int main(int argc, char** argv) {

  ${preparation._1}

  //measure time
 	struct timespec tp_start;
	struct timespec tp_end;
	clockid_t clk_id = CLOCK_MONOTONIC;
  double duration = 0;

  clock_gettime(clk_id, &tp_start);
  ${preparation._3}
  clock_gettime(clk_id, &tp_end);

  duration = (tp_end.tv_sec - tp_start.tv_sec) * 1000000000 + (tp_end.tv_nsec - tp_start.tv_nsec);
  duration = duration / 1000000;

  //add checking code here

  ${preparation._2}

  printf("%f", duration);

  return duration;
}
"""

    testCode
  }

  def compileAndExecute(code:String):Double = {
    try {
      val returnValue = Execute2(code)
      println("result: " + returnValue)
      returnValue.toDouble
    }catch {
      case e: Throwable => println("execution failed")
        -1
    }
  }


}
