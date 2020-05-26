package myapp

import mylibrary.Show

enum Employee derives Show {
  case Grunt(name: String, boss: Employee)
  case CEO
}
