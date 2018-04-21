-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
    deriving (Show)
data MagazineInfo = Magazine Int String [String]
    deriving (Show)
myInfo = Book 43 "Algebra I" ["Mathematiker I","Mathematiker 2"] 

data BookReview = BookReview BookInfo CustomerID String
    deriving (Show)

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
    | CashOnDelivery
    | Invoice CustomerID
    deriving (Show)
    
    
bookID (Book id _ _ ) = id
bookTitle (Book _ title _ ) = title
bookAuthors (Book _ _ authors) = authors

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: String,
    customerAddress :: Address
    }
    deriving (Show)

customer1 = Customer 12 "myNAme" ["myAddress1","myAddress2"]

customer2 = Customer {
    customerID = 77,
    customerAddress = ["anotherAddress1","anotherAddress2"],
    customerName = "Awesome Dude"
    }
    