module data

enum Rhythm {weekly, byweekly_even, byweekly_odd}

enum Semester {sem1, sem2, sem3, sem4, sem5, sem6}
enum Module {MODULE}

abstract sig ParentGroupUnit {}

abstract sig Department {name:String}

one sig ger extends Department {} {name= "Germanistik"}
one sig ang extends Department {} {name= "Anglistik"}
one sig kom extends Department {} {name= "Kommunikationswissenschaften"}
one sig lin extends Department {} {name= "Linguistik"}
one sig dep1 extends Department {} {name= "Name1"}
one sig soz extends Department {} {name= "Soziologie"}
one sig ges extends Department {} {name= "Geschichte"}
one sig pol extends Department {} {name= "Politologie"}
one sig dep2 extends Department {} {name= "Name2"}
one sig rom extends Department {} {name= "Romanistik"}
one sig jud extends Department {} {name= "Jüdische Studien"}
one sig inf extends Department {} {name= "Informationswissenschaften"}
one sig jap extends Department {} {name= "Modernes Japan"}
one sig jid extends Department {} {name= "Jiddistik"}

enum Course {course1_course2}

enum Slot {a1,a2,a3,a4,a5,
            b1,b2,b3,b4,b5,
            c1,c2,c3,c4,c5,
            d1,d2,d3,d4,d5,
            e1,e2,e3,e4,e5,
            f1,f2,f3,f4,f5,
            g1,g2,g3,g4,g5}

abstract sig Session {
  rhythm: one Int,
  slot: one Slot,
  duration: one Int

} {
  rhythm in 0 + 1 + 2 + 3
  duration in 1 + 2 + 3
}

one sig session1 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session2 extends Session {} {
  rhythm=0
  duration=2
  slot=a3
}
one sig session3 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session4 extends Session {} {
  rhythm=0
  duration=2
  slot=a3
}
one sig session5 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session6 extends Session {} {
  rhythm=0
  duration=2
  slot=a3
}
one sig session7 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session8 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session9 extends Session {} {
  rhythm=0
  duration=2
  slot=a4
}
one sig session10 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session11 extends Session {} {
  rhythm=0
  duration=2
  slot=a3
}
one sig session12 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}
one sig session13 extends Session {} {
  rhythm=0
  duration=2
  slot=a2
}


abstract sig Group extends ParentGroupUnit {
  title: String,
  sessions: some Session
}


one sig group1 extends Group {} {
  title = "Title 4-I"
  sessions = session1
}
one sig group2 extends Group {} {
  title = "Title 4-I"
  sessions = session2
}

one sig group3 extends Group {} {
  title = "Title 3-I"
  sessions = session3
}
one sig group4 extends Group {} {
  title = "Title 3-I"
  sessions = session4
}

one sig group5 extends Group {} {
  title = "Title 1-I"
  sessions = session5
}
one sig group6 extends Group {} {
  title = "Title 1-I"
  sessions = session6
}

one sig group7 extends Group {} {
  title = "Title 5-I"
  sessions = session7
}

one sig group8 extends Group {} {
  title = "Title 4-II"
  sessions = session8
}

one sig group9 extends Group {} {
  title = "Title 3-III"
  sessions = session9
}

one sig group10 extends Group {} {
  title = "Title 5-II"
  sessions = session10
}

one sig group11 extends Group {} {
  title = "Title 1-III"
  sessions = session11
}

one sig group12 extends Group {} {
  title = "Title 1-II"
  sessions = session12
}

one sig group13 extends Group {} {
  title = "Title 3-II"
  sessions = session13
}


abstract sig Unit extends ParentGroupUnit {
  titleUnit: String,
  department: one Department,
  groups: set Group,
}
/* add a group signature for each each group instead of using integers */
/* explore in vs extends page 111 */

one sig unit1 extends Unit {} {
  titleUnit= "Title 4-I"
  department= dep1
  groups = group1 + group2
}
one sig unit2 extends Unit {} {
  titleUnit= "Title 3-I"
  department= dep1
  groups = group3 + group4
}
one sig unit3 extends Unit {} {
  titleUnit= "Title 1-I"
  department= dep1
  groups = group5 + group6
}
one sig unit4 extends Unit {} {
  titleUnit= "Title 5-I"
  department= dep1
  groups = group7
}
one sig unit5 extends Unit {} {
  titleUnit= "Title 4-II"
  department= dep2
  groups = group8
}
one sig unit6 extends Unit {} {
  titleUnit= "Title 3-III"
  department= dep2
  groups = group9
}
one sig unit7 extends Unit {} {
  titleUnit= "Title 5-II"
  department= dep2
  groups = group10
}
one sig unit8 extends Unit {} {
  titleUnit= "Title 1-III"
  department= dep2
  groups = group11
}
one sig unit9 extends Unit {} {
  titleUnit= "Title 1-II"
  department= dep2
  groups = group12
}
one sig unit10 extends Unit {} {
  titleUnit= "Title 3-II"
  department= dep2
  groups = group13
}

let mapping = course1_course2 -> sem3 -> unit2 +
        course1_course2 -> sem1 -> unit9 +
        course1_course2 -> sem4 -> unit1 +
        course1_course2 -> sem5 -> unit7 +
        course1_course2 -> sem3 -> unit10 +
        course1_course2 -> sem3 -> unit6 +
        course1_course2 -> sem1 -> unit3 +
        course1_course2 -> sem4 -> unit5 +
        course1_course2 -> sem5 -> unit4 +
        course1_course2 -> sem1 -> unit8

pred show () {}
run show

