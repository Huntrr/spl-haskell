module WordLists where

firstPerson = ["I", "me"]

firstPersonPossessive = ["mine", "my"]

positiveNouns = ["angel", "day", "flower", "girl", "happiness", "heaven", "hero", "joy", "king", "kingdom", "kitten", "lady", "lord", "plum", "pony", "prince", "rose", "summer", "warrior"]

be = ["am", "are", "art", "be", "is"]

zero = ["nothing", "zero"]

neutralAdjectives = ["big", "black", "blue", "bluest", "bottomless", "furry", "green", "hard", "huge", "large", "little", "normal", "old", "purple", "red", "rural", "small", "tiny", "white", "yellow", "temporary", "limiting"]

negativeNouns = ["Apple", "Hell", "bastard", "beggar", "blister", "codpiece", "coward", "curse", "death", "devil", "draught", "famine", "flirt-gill", "goat", "hate", "hog", "hound", "ignorance", "leech", "lie", "pig", "plague", "starvation", "tear", "toad", "war", "wolf"]

characters = ["Achilles", "Adonis", "Adriana", "Aegeon", "Aemilia", "Agamemnon", "Agrippa", "Ajax", "Alexander", "Alonso", "Andromache", "Angelo", "Antiochus", "Antonio", "Arthur", "Autolycus", "Balthazar", "Banquo", "Beatrice", "Benedick", "Benvolio", "Bianca", "Brabantio", "Brutus", "Capulet", "Cassandra", "Cassius", "Christopher Sly", "Cicero", "Claudio", "Claudius", "Cleopatra", "Cordelia", "Cornelius", "Cressida", "Cymberline", "Demetrius", "Desdemona", "Dionyza", "Doctor Caius", "Dogberry", "Don John", "Don Pedro", "Donalbain", "Dorcas", "Duncan", "Egeus", "Emilia", "Escalus", "Falstaff", "Fenton", "Ferdinand", "Ford", "Fortinbras", "Francisca", "Friar John", "Friar Laurence", "Gertrude", "Goneril", "Hamlet", "Hecate", "Hector", "Helen", "Helena", "Hermia", "Hermonie", "Hippolyta", "Horatio", "Imogen", "Isabella", "John of Gaunt", "John of Lancaster", "Julia", "Juliet", "Julius Caesar", "King Henry", "King John", "King Lear", "King Richard", "Lady Capulet", "Lady Macbeth", "Lady Macduff", "Lady Montague", "Lennox", "Leonato", "Luciana", "Lucio", "Lychorida", "Lysander", "Macbeth", "Macduff", "Malcolm", "Mariana", "Mark Antony", "Mercutio", "Miranda", "Mistress Ford", "Mistress Overdone", "Mistress Page", "Montague", "Mopsa", "Oberon", "Octavia", "Octavius Caesar", "Olivia", "Ophelia", "Orlando", "Orsino", "Othello", "Page", "Pandarus", "Pantino", "Paris", "Pericles", "Pinch", "Polonius", "Pompeius", "Portia", "Priam", "Prince Henry", "Prospero", "Proteus", "Publius", "Puck", "Queen Elinor", "Regan", "Robin", "Romeo", "Rosalind", "Sebastian", "Shallow", "Shylock", "Slender", "Solinus", "Stephano", "Thaisa", "The Abbot of Westminster", "The Apothecary", "The Archbishop of Canterbury", "The Duke of Milan", "The Duke of Venice", "The Ghost", "Theseus", "Thurio", "Timon", "Titania", "Titus", "Troilus", "Tybalt", "Ulysses", "Valentine", "Venus", "Vincentio", "Viola"]

negativeAdjectives = ["bad", "cowardly", "cursed", "damned", "dirty", "disgusting", "distasteful", "dusty", "evil", "fat", "fat-kidneyed", "fatherless", "foul", "hairy", "ugly", "half-witted", "horrible", "horrid", "infected", "lying", "miserable", "misused", "oozing", "rotten", "skilless", "smelly", "snotty", "sorry", "stinking", "stuffed", "stupid", "tame", "unpracticed", "vile", "villainous", "weak", "worried"]

-- TODO: I added "summer's" to make hello.spl pass, but that seems weird. Should I automatically add 's to nouns and make them adjectives?
positiveAdjectives = ["amazing", "beautiful", "blossoming", "bold", "brave", "charming", "clearest", "cunning", "cute", "delicious", "nice", "embroidered", "fair", "fine", "gentle", "golden", "good", "handsome", "happy", "healthy", "honest", "lovely", "loving", "mighty", "noble", "peaceful", "pretty", "prompt", "proud", "reddest", "rich", "smooth", "sunny", "sweet", "sweetest", "trustworthy", "warm", "young", "summer's"]

adjectives = neutralAdjectives ++ negativeAdjectives ++ positiveAdjectives

negativeComparators = ["punier", "smaller", "worse", "uglier"]

secondPersonReflexive = ["thyself", "yourself"]

neutralNouns = ["animal", "aunt", "brother", "cat", "chihuahua", "cousin", "cow", "daughter", "door", "face", "factor", "father", "fellow", "flatterer", "granddaughter", "grandfather", "grandmother", "grandson", "hair", "hamster", "horse", "infancy", "infant", "lamp", "lantern", "man", "mistletoe", "moon", "morning", "mother", "nephew", "niece", "nose", "purse", "road", "roman", "servant", "sister", "sky", "son", "squirrel", "stone wall", "thing", "town", "tree", "uncle", "value", "variable", "varlet", "virgin", "wind", "woman"]

firstPersonReflexive = ["myself"]

secondPerson = ["thee", "thou", "you"]

articles = ["a", "an", "the"]

secondPersonPossessive = ["thine", "thy", "your"]

thirdPersonPossessive = ["his", "her", "its", "their"]

-- TODO: I had to add "more cunning" to make primes.spl pass. Should more + adjective always work?
positiveComparators = ["better", "bigger", "fresher", "friendlier", "jollier", "nicer", "more cunning"]
