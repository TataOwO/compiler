class A {
    String x;
    int y;
    boolean z;
    A next;
    A() {}
}
class Main {
    public static void main(String args[]) {
        A a = new A();
        if (a.x == null)
            System.out.print("x = null\n");
        System.out.print("y = " + a.y + "\n");
        if (!a.z)
            System.out.print("z = false\n");
        if (a.next == null)
            System.out.print("next = null\n");
    }
}

