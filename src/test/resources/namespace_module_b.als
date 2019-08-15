module moduleB

  sig Person {
      favorite: Book,
      knows: set Person,
      private likes: set Person
  }

  private sig Book { year: Int }

