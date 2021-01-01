open! Core_kernel
open Scattegories_common

let all =
  {|
4-Letter Words
A Boy's Name
Acronyms
A Girl's Name
Alcoholic Drinks
Animals
Animals in Books or Movies
Appliances
Authors
Awards/Ceremonies
Bad Habits
Birds
Board Games
Bodies of Water
Book Titles
Breakfast Foods
Capitals
Cars
Cartoon Characters
Celebrities
Childrens Books
Christmas Songs
Cities
Cities in the US
Colours
Companies
Computer Parts
Computer Programs
Cooking Utensils
Cosmetics/Toiletries
Countries
Crimes
Dairy Products
Desserts
Diet Foods
Diseases
Electronic Gadgets
Ethnic Foods
Excuses for Being Late
Famous Females
Farm Animals
Fictional Characters
Films
Fish
Flowers
Food/Drink That Is Green
Foods You Eat Raw
Footwear
Foreign Cities
Foreign Words Used in English
Fruits
Furniture
Games
Game Terms
Gifts/Presents
Halloween Costumes
Historical Figures
Hobbies
Honeymoon Spots
Household Chores
Ice Cream Flavors
Insects
Items in a Catalogue
Items in a Kitchen
Items in a Refrigerator
Items in a Suitcase
Items in a Vending Machine
Items You Save Up to Buy
Items You Take on a Trip
Junk Food
Kinds of Dances
Kinds of Soup
Leisure Activities
Magazines
Medicine/Drugs
Menu Items
Methods of Transportation
Musical Groups
Musical Instruments
Names Used in Songs
Names Used in the Bible
Nicknames
Occupations
Offensive Words
Olympic Events
Parks
Parts of the Body
People in Uniform
Personality Traits
Pizza Toppings
Places in Asia
Places to Hang Out
Presidents
Product Names
Reasons to Call 999
Reasons to Make a Phone Call
Reasons to Take Out a Loan
Reptiles/Amphibians
Restaurants
Sandwiches
School Subjects
School Supplies
Seafood
Software
Something You Keep Hidden
Something You're Afraid Of
Song Titles
Spices/Herbs
Sports Equipment
Sports Played Indoors
States
Stones/Gems
Store Names
Street Names
Sweet Things
Television Stars
Terms from Maths
Terms of Endearment
Terms of Measurement
Things at a Carnival
Things at a Circus
Things at a Football Game
Things Found in a Desk
Things Found in a Pub
Things Found in New York
Things Found on a Map
Things in a Medicine Cabinet
Things in a Park
Things in a Souvenir Shop
Things in a Supermarket
Things in the Sky
Things Made of Metal
Things on a Beach
Things That Are Black
Things That Are Cold
Things That Are Found in the Ocean
Things That Are Round
Things That Are Sticky
Things That Can Get You Fired
Things That Can Kill You
Things That Grow
Things That Have Buttons
Things That Have Spots
Things That Have Stripes
Things That Jump/Bounce
Things That Make You Smile
Things That Use a Remote
Things That You Wear
Things to Do at a Party
Things to Do on a Date
Things With Tails
Things You Do Every Day
Things You Get in the Mail
Things You Get Tickets For
Things You Make
Things You Plug In
Things You're Allergic To
Things You Replace
Things You Save Up to Buy
Things You See at the Zoo
Things You Shout
Things You Sit In/On
Things You Store Items In
Things You Throw Away
Titles People Can Have
Tools
Tourist Attractions
Trees
Tropical Locations
T.V. Shows
Types of Drink
Types of Drinks
Types of Weather
Universities
University Subjects
Vacation Spots
Vegetables
Vehicles
Video Games
Villains/Monsters
Ways to Kill Time
Websites
Weekend Activities
Wireless Things
Words Associated With Exercise
Words Associated With Money
Words Associated With Winter
Words Ending in -N
Words With Double Letters
World Leaders/Politicians
   |}
  |> String.split ~on:'\n'
  |> List.filter_map ~f:(fun category ->
         if String.is_empty category then None else Some (Category_id.of_string category))
  |> Category_id.Set.of_list
;;
