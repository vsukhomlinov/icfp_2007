package rna;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class Parser {


    public static Queue<RNA> parseRna (String fileName) throws Exception {
        BufferedReader reader = new BufferedReader(new FileReader(fileName));
        char[] chunk = new char[7];
        Deque<RNA> stringStack = new ArrayDeque<RNA>();
        while (reader.ready()) {
            if (reader.read(chunk) < 7) {
                break;
            }
            RNA rna = RNA.fromString(new String(chunk));
            if (rna == null) {
                //something we can't recognize
                System.out.println("Wrong sequence :"+ new String(chunk));
                continue;
            }
            stringStack.addLast(rna);
        }
        return stringStack;
    }
}
