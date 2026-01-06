class A {
    String x;
    int y;
    boolean z;
    A next;
    A() {
        x = "toto";
        y = 42;
        z = true;
        next = this;
    }
}
class Main {
    public static void main(String args[]) {
        A a = new A();
        System.out.print("x = " + a.x + "\n");
        System.out.print("y = " + a.y + "\n");
        if (a.z)
            System.out.print("z = true\n");
        if (a.next == a)
            System.out.print("next = a\n");
    }
}

