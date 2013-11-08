package rna;

/**
* Created with IntelliJ IDEA.
* User: vitaly
* Date: 10/15/13
* Time: 9:35 AM
* To change this template use File | Settings | File Templates.
*/
enum DIR {

    N(0,599),
    W(599,0),
    S(0,601),
    E(601,0);

    public int x;
    public int y;

    DIR(int x, int y) {
        //To change body of created methods use File | Settings | File Templates.
        this.x = x;
        this.y = y;
    }

    public static DIR nextCCW(DIR direction) {
        if (direction == DIR.E) return direction = DIR.N;
        if (direction == DIR.S) return direction = DIR.E;
        if (direction == DIR.W) return direction = DIR.S;
        return direction = DIR.W;
    }

    public static DIR nextCW(DIR direction) {
        if (direction == DIR.E) return direction = DIR.S;
        if (direction == DIR.S) return direction = DIR.W;
        if (direction == DIR.W) return direction = DIR.N;
        return direction = DIR.E;
    }


}
