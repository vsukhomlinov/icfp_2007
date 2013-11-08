package rna;

/**
* Created with IntelliJ IDEA.
* User: vitaly
* Date: 10/15/13
* Time: 9:35 AM
* To change this template use File | Settings | File Templates.
*/
class Position {

    int x;
    int y;

    private DIR direction = DIR.E;

    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public void move() {
        x = (x + direction.x) % Draw.IMAGE_SIZE;
        y = (y + direction.y) % Draw.IMAGE_SIZE;
    }

    public void turnCCW() {
        direction = DIR.nextCCW(direction);
    }

    public void turnCW() {
         direction = DIR.nextCW(direction);
    }

    public Position copy() {
        return new Position(x,y);
    }

    @Override
    public String toString() {
        return String.format("(%s,%s)", x, y);
    }
}
