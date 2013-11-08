package rna;

import java.io.File;
import java.util.Queue;

/**
 * Created with IntelliJ IDEA.
 * User: vitaly
 * Date: 10/15/13
 * Time: 12:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class RnaProcessor {

    public static void main(String[] args) throws Exception {
        Queue<RNA> queue = Parser.parseRna("tmp/rna.txt");

        RNA chunk;
        Draw drawer = new Draw();
        while ((chunk = queue.poll()) != null) {
//            System.out.println(chunk);
              drawer.draw(chunk);
        }

        drawer.drawFile(new File("tmp/rna.png"));

    }
}
