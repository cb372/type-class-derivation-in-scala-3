package myapp

import mylibrary.Show

case class Pet(
  name: String,
  species: String,
  owner: Person
) derives Show
