See src/dj/README.clj for example

As a step to respond to the push to develop sophisticated web
applications, Rich Hickey created Clojurescript. Currently, much of
the core of web development comes from jquery and does not fit well
with the google closure model. Having tools written in
Clojure/Clojurescript would be more ideal by improving extensibility
and the ability to be interfaced by users.

Coming from the philosophy of increasing productivity by minimizing
the delay from feedback from the changes we make to our designs, I am
interested in developing a clojurescript based editor. As hinted
earlier, there is a lack of primitives for web based editors that are
google closure friendly. I thus present a simple example of an editor
along with the primitives used to make it.