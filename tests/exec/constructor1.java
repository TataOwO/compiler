class A {
    int x;
    int y;
    A (int x, int y) { this.x = x; this.y =y; } }

class Main {
    public static void main(String args[]) {
	A a = new A(17, 42);
        System.out.print(a.x + "\n");
        System.out.print(a.y + "\n");
    }
}
