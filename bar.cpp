namespace Foo {
  bool hi() {
    if (true && false) {
      return true;
    } else if (true || false) {
      return true;
    } else {
      return true;
    }

    if (true && false) {
      return true;
    }

    if (true && false) {
      return true;
    } else if (true || false) {
      return false;
    }
  }
}
