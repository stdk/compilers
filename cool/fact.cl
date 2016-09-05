class Main inherits IO {

  conv : A2I <- new A2I; 
  i : Int;
  
  fact(i : Int):Int {
    let f: Int <- 1 in {
      while (not (i = 0)) loop {
        f <- f*i;
        i <- i-1;
      } pool;
      f;     
    }
  };

  main():Object { 
    {
      i <- conv.a2i(in_string());  
      out_string(conv.i2a(fact(i)).concat("\n"));
    }
  };
};

