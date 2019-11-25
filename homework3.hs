--description of types
type Name = String
newtype Songs = Song Name deriving Show
data Alboms = Albom Name [Songs] deriving Show
data Groups = Group Name [Alboms] deriving Show
data Users = User Name [Songs] [Alboms] [Groups] deriving Show

--function of find tracks by group
getTrackByGroup :: Groups -> [Songs]
getTrackByGroup (Group _ [Albom _ so]) = so

--function of add new group
addGroup :: Users -> Groups -> Users
addGroup (User na so al gr) gro = User na so al (gro:gr)

--function of add new albom
addAlbom :: Users -> Alboms -> Users
addAlbom (User na so al gr) alb = User na so (alb:al) gr

--function of add new song
addSong :: Users -> Songs -> Users
addSong (User na so al gr) son = User na (son:so) al gr

--- examples
song1 = Song "Limits of dream"
song2 = Song "Never"
song3 = Song "When you fall?"
song4 = Song "Far coat"
albom1 = Albom "2019" [song1, song2, song3]
albom2 = Albom "2013" [song4]
group1 = Group "Musicant says" [albom1]
group2 = Group "IandN" [albom2]
user1 = User "Nikita" [song1, song3] [albom1, albom2] [group1, group2]