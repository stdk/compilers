class List inherits A2I {
    item: Object;
    next: List;

    init(i: Object, n: List):List {
        {
            item <- i;
            next <- n;
            self;
        }
    };

    flatten(): String {
        let string: String <- 
            case item of
                i: Int => i2a(i);
                s: String => s;
                l: List => l.flatten();
                o: Object => { abort(); ""; };
            esac
        in
            if(isvoid next) then
                string
            else
                string.concat(next.flatten())
            fi
    };
};

class Main inherits IO {

    main(): Object {
        let nil: List,
            hello: String <- "Hello ",
            world: String <- "World!",
            newline: String <- "\n",
            four: Int <- 4,
            two: Int <- 2,
            string_list: List <- (new List).init(hello,
                              (new List).init(world,nil)),
            int_list: List <- (new List).init(four,
                                (new List).init(two,nil)),
            list: List <- (new List).init(string_list,
                            (new List).init(" ",
                                (new List).init(int_list,
                                    (new List).init(newline,nil))))
        in 
            out_string(list.flatten())
    };
};