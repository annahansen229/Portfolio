import "bootstrap/dist/css/bootstrap.min.css";
import "./App.css";
import Image from "../../harry-potter-app/src/images/hedwig.gifv";
import Image2 from "../../harry-potter-app/src/images/boats.gif";
//import Image3 from "../../harry-potter-app/src/images/boats.gif";
import "../../harry-potter-app/src/components/sortinghat";

import Navbar from "./components/navbar";

export default function App() {
  
  return (
    <div className="App">
      <Navbar />
      <h1 class="frontpage">The adventure begins...</h1>
     
        <figure>
          <img
            src="https://media.tumblr.com/tumblr_m9laumf6Wi1royzou.gif"
            width="100%"
            alt="hogwarts letter gif"
          />
          <figcaption>Hogwarts Letter</figcaption>
        </figure>
        <figure>
          <img src={Image} width="100%" alt="hedwig gif" />
          <figcaption>Hedwig</figcaption>
        </figure>
        <figure>
          <img src={Image2} width="100%" alt="hogwarts castle gif" />
          <figcaption>Hogwarts by Boat</figcaption>
        </figure>
        <figure>
          <img
            src="https://64.media.tumblr.com/03dda28570b5f046ccdcfb0be9a44fdf/3ce07900af64d492-e1/s500x750/713b27469330dfb041ea89b9d39e756e33cd70c9.gifv"
            width="100%"
            alt="Sorting Hat gif"
          />
          <figcaption>Sorting Hat</figcaption>
        </figure>

        {/*Implementing the Gryffindor House Quiz*/}
        <h2> Now it is your turn! </h2>
        <form action="">
          <fieldset>
            <legend class="legend">Sorting Hat Quiz</legend>
            <legend>Q1: Dawn or Dusk?</legend>
            <div>
              <input type="radio" id="dawn" name="question1" value="dawn" />
              <label for="dawn">Dawn</label>
            </div>
            <div>
              <input type="radio" id="dusk" name="question1" value="dusk" />
              <label for="dusk">Dusk</label>
            </div>
            <div>
              <input type="radio" id="blank1" name="question1" value="blank" />
              <label for="blank1">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q2: Forest or River?</legend>
            <div>
              <input type="radio" id="forest" name="question2" value="forest" />
              <label for="forest">Forest</label>
            </div>
            <div>
              <input type="radio" id="river" name="question2" value="river" />
              <label for="river">River</label>
            </div>
            <div>
              <input type="radio" id="blank2" name="question2" value="blank" />
              <label for="blank2">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q3:Moon or Stars?</legend>
            <div>
              <input type="radio" id="moon" name="question3" value="moon" />
              <label for="moon">Moon</label>
            </div>
            <div>
              <input type="radio" id="stars" name="question3" value="stars" />
              <label for="stars">Stars</label>
            </div>
            <div>
              <input type="radio" id="blank3" name="question3" value="blank" />
              <label for="blank3">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q4:Which of the following would most people hate to call you?
            </legend>
            <div>
              <input
                type="radio"
                id="ordinary"
                name="question4"
                value="ordinary"
              />
              <label for="ordinary">Ordinary</label>
            </div>
            <div>
              <input
                type="radio"
                id="ignorant"
                name="question4"
                value="ignorant"
              />
              <label for="ignorant">Ignorant</label>
            </div>
            <div>
              <input
                type="radio"
                id="cowardly"
                name="question4"
                value="cowardly"
              />
              <label for="cowardly">Cowardly</label>
            </div>
            <div>
              <input
                type="radio"
                id="selfish"
                name="question4"
                value="selfish"
              />
              <label for="selfish">Selfish</label>
            </div>
            <div>
              <input type="radio" id="blank4" name="question4" value="blank" />
              <label for="blank4">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q5:After you have died, what would you most like people to do when
              they hear your name?
            </legend>
            <div>
              <input type="radio" id="miss" name="question5" value="miss" />
              <label for="miss">Miss you, but smile</label>
            </div>
            <div>
              <input
                type="radio"
                id="stories"
                name="question5"
                value="stories"
              />
              <label for="stories">
                Ask for more stories about your adventures
              </label>
            </div>
            <div>
              <input type="radio" id="think" name="question5" value="think" />
              <label for="think">
                Think with admiration of your achievements
              </label>
            </div>
            <div>
              <input type="radio" id="i" name="question5" value="i" />
              <label for="i">
                I don't care what people think of me after I'm dead, it's what
                they think of me while I'm alive that counts
              </label>
            </div>
            <div>
              <input type="radio" id="blank5" name="question5" value="blank" />
              <label for="blank5">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q6:How would you like to be known to history?</legend>
            <div>
              <input type="radio" id="wise" name="question6" value="wise" />
              <label for="wise">The Wise</label>
            </div>
            <div>
              <input type="radio" id="good" name="question6" value="good" />
              <label for="good">The Good</label>
            </div>
            <div>
              <input type="radio" id="great" name="question6" value="great" />
              <label for="great">The Great</label>
            </div>
            <div>
              <input type="radio" id="bold" name="question6" value="bold" />
              <label for="bold">The Bold</label>
            </div>
            <div>
              <input type="radio" id="blank6" name="question6" value="blank" />
              <label for="blank6">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q7:Given the choice, would you rather invent a potion that would
              guarantee you:
            </legend>
            <div>
              <input type="radio" id="love" name="question7" value="love" />
              <label for="love">Love?</label>
            </div>
            <div>
              <input type="radio" id="glory" name="question7" value="glory" />
              <label for="glory">Glory?</label>
            </div>
            <div>
              <input type="radio" id="wisdom" name="question7" value="wisdom" />
              <label for="wisdom">Wisdom?</label>
            </div>
            <div>
              <input type="radio" id="power" name="question7" value="power" />
              <label for="power">Power?</label>
            </div>
            <div>
              <input type="radio" id="blank7" name="question7" value="blank" />
              <label for="blank7">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q8:Once every century, the Flutterby bush produces flowers that
              adapt their scent to attract the unwary. If it lured you, it would
              smell of:
            </legend>
            <div>
              <input type="radio" id="fire" name="question8" value="fire" />
              <label for="fire">A crackling log fire</label>
            </div>
            <div>
              <input type="radio" id="sea" name="question8" value="sea" />
              <label for="sea">The sea</label>
            </div>
            <div>
              <input
                type="radio"
                id="parchment"
                name="question8"
                value="parchment"
              />
              <label for="parchment">Fresh parchment</label>
            </div>
            <div>
              <input type="radio" id="home" name="question8" value="home" />
              <label for="home">Home</label>
            </div>
            <div>
              <input type="radio" id="blank8" name="question8" value="blank" />
              <label for="blank8">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q9:Four goblets are placed before you. Which would you choose to
              drink?
            </legend>
            <div>
              <input
                type="radio"
                id="diamonds"
                name="question9"
                value="diamonds"
              />
              <label for="diamonds">
                The foaming, frothing, silvery liquid that sparkles as though
                containing ground diamonds
              </label>
            </div>
            <div>
              <input type="radio" id="plums" name="question9" value="plums" />
              <label for="plums">
                The smooth, thick, richly purple drink that gives off a
                delicious smell of chocolate and plums
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="sunspots"
                name="question9"
                value="sunspots"
              />
              <label for="sunspots">
                The golden liquid so bright that it hurts the eye, and which
                makes sunspots dance all around the room
              </label>
            </div>
            <div>
              <input type="radio" id="ink" name="question9" value="ink" />
              <label for="ink">
                The mysterious black liquid that gleams like ink, and gives off
                fumes that make you see strange visions
              </label>
            </div>
            <div>
              <input type="radio" id="blank9" name="question9" value="blank" />
              <label for="blank9">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q10:What kind of instrument most pleases your ear?</legend>
            <div>
              <input
                type="radio"
                id="violin"
                name="question10"
                value="violin"
              />
              <label for="violin">The violin</label>
            </div>
            <div>
              <input
                type="radio"
                id="trumpet"
                name="question10"
                value="trumpet"
              />
              <label for="trumpet">The trumpet</label>
            </div>
            <div>
              <input type="radio" id="piano" name="question10" value="piano" />
              <label for="piano">The piano</label>
            </div>
            <div>
              <input type="radio" id="drum" name="question10" value="drum" />
              <label for="drum">The drum</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank10"
                name="question10"
                value="blank"
              />
              <label for="blank10">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q11:You enter an enchanted garden. What would you be most curious
              to examine first?
            </legend>
            <div>
              <input
                type="radio"
                id="apples"
                name="question11"
                value="apples"
              />
              <label for="apples">
                The silver leafed tree bearing golden apples
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="toadstools"
                name="question11"
                value="toadstools"
              />
              <label for="toadstools">
                The fat red toadstools that appear to be talking to each other
              </label>
            </div>
            <div>
              <input type="radio" id="pool" name="question11" value="pool" />
              <label for="pool">
                The bubbling pool, in the depths of which something luminous is
                swirling
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="statue"
                name="question11"
                value="statue"
              />
              <label for="statue">
                The statue of an old wizard with a strangely twinkling eye
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank11"
                name="question11"
                value="blank"
              />
              <label for="blank11">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q12:Four boxes are placed before you. Which would you try and
              open?
            </legend>
            <div>
              <input
                type="radio"
                id="tortoiseshell"
                name="question12"
                value="tortoiseshell"
              />
              <label for="tortoiseshell">
                The small tortoiseshell box, embellished with gold, inside which
                some small creature appears to be squeaking
              </label>
            </div>
            <div>
              <input type="radio" id="rune" name="question12" value="rune" />
              <label for="rune">
                The gleaming jet black box with a silver lock and key, marked
                with a mysterious rune that you know to be the mark of Merlin
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="ornate"
                name="question12"
                value="ornate"
              />
              <label for="ornate">
                The ornate golden casket, standing on clawed feet, whose
                inscription warns that both secret knowledge and unbearable
                temptation lie within
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="pewter"
                name="question12"
                value="pewter"
              />
              <label for="pewter">
                The small pewter box, unassuming and plain, with a scratched
                message upon it that reads 'I open only for the worthy'
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank12"
                name="question12"
                value="blank"
              />
              <label for="blank12">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q13:A troll has gone beserk in the Headmaster's study at Hogwarts.
              It is about to smash, crush and tear several irreplaceable items
              and treasures. In which order would you rescue these objects from
              the troll's club, if you could?
            </legend>
            <div>
              <input type="radio" id="crb" name="question13" value="crb" />
              <label for="crb">
                First, a nearly perfected cure for dragon pox. Then student
                records going back 1000 years. Finally, a mysterious handwritten
                book full of strange runes
              </label>
            </div>
            <div>
              <input type="radio" id="rbc" name="question13" value="rbc" />
              <label for="rbc">
                First, student records going back 100 years. Then a mysterious
                handwritten book full of strange runes. Finally, a nearly
                perfected cure for dragon pox
              </label>
            </div>
            <div>
              <input type="radio" id="bcr" name="question13" value="bcr" />
              <label for="bcr">
                First, a mysterious handwritten book full of strange runes. Then
                a nearly perfected cure for dragon pox. Finally, student records
                going back 1000 years
              </label>
            </div>
            <div>
              <input type="radio" id="cbr" name="question13" value="cbr" />
              <label for="cbr">
                First, a nearly perfected cure for dragon pox. Then a mysterious
                handwritten book full of strange runes. Finally, student records
                going back 1000 years
              </label>
            </div>
            <div>
              <input type="radio" id="rcb" name="question13" value="rcb" />
              <label for="rcb">
                First student records going back 1000 years. Then, a nearly
                perfected cure for dragon pox. Finally, a mysterious handwritten
                book full of strange runes
              </label>
            </div>
            <div>
              <input type="radio" id="brc" name="question13" value="brc" />
              <label for="brc">
                First, a mysterious handwritten book full of strange runes. Then
                student records going back 1000 years. Finally, a nearly
                perfected cure for dragon pox
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank13"
                name="question13"
                value="blank"
              />
              <label for="blank13">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q14:Which of the following do you find most difficult to deal
              with?
            </legend>
            <div>
              <input
                type="radio"
                id="hunger"
                name="question14"
                value="hunger"
              />
              <label for="hunger">Hunger</label>
            </div>
            <div>
              <input type="radio" id="cold" name="question14" value="cold" />
              <label for="cold">Cold</label>
            </div>
            <div>
              <input
                type="radio"
                id="loneliness"
                name="question14"
                value="loneliness"
              />
              <label for="loneliness">Loneliness</label>
            </div>
            <div>
              <input
                type="radio"
                id="boredom"
                name="question14"
                value="boredom"
              />
              <label for="boredom">Boredom</label>
            </div>
            <div>
              <input
                type="radio"
                id="ignored"
                name="question14"
                value="ignored"
              />
              <label for="ignored">Being ignored</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank14"
                name="question14"
                value="blank"
              />
              <label for="blank14">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q15:Which would you rather be:</legend>
            <div>
              <input
                type="radio"
                id="envied"
                name="question15"
                value="envied"
              />
              <label for="envied">Envied?</label>
            </div>
            <div>
              <input
                type="radio"
                id="imitated"
                name="question15"
                value="imitated"
              />
              <label for="imitated">Imitated?</label>
            </div>
            <div>
              <input
                type="radio"
                id="trusted"
                name="question15"
                value="trusted"
              />
              <label for="trusted">Trusted?</label>
            </div>
            <div>
              <input
                type="radio"
                id="praised"
                name="question15"
                value="praised"
              />
              <label for="praised">Praised?</label>
            </div>
            <div>
              <input type="radio" id="liked" name="question15" value="liked" />
              <label for="liked">Liked?</label>
            </div>
            <div>
              <input
                type="radio"
                id="feared"
                name="question15"
                value="feared"
              />
              <label for="feared">Feared?</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank15"
                name="question15"
                value="blank"
              />
              <label for="blank15">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q16:If you could have any power, which would you choose?
            </legend>
            <div>
              <input type="radio" id="minds" name="question16" value="minds" />
              <label for="minds">The power to read minds</label>
            </div>
            <div>
              <input
                type="radio"
                id="invisibility"
                name="question16"
                value="invisibility"
              />
              <label for="invisibility">The power of invisibility</label>
            </div>
            <div>
              <input
                type="radio"
                id="strength"
                name="question16"
                value="strength"
              />
              <label for="strength">The power of superhuman strength</label>
            </div>
            <div>
              <input
                type="radio"
                id="animals"
                name="question16"
                value="animals"
              />
              <label for="animals">The power to speak to animals</label>
            </div>
            <div>
              <input type="radio" id="past" name="question16" value="past" />
              <label for="past">The power to change the past</label>
            </div>
            <div>
              <input
                type="radio"
                id="appearance"
                name="question16"
                value="appearance"
              />
              <label for="appearance">
                The power to change your appearance at will
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank16"
                name="question16"
                value="blank"
              />
              <label for="blank16">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q17:What are you most looking forward to learning at Hogwarts?
            </legend>
            <div>
              <input
                type="radio"
                id="apparition"
                name="question17"
                value="apparition"
              />
              <label for="apparition">
                Apparition and Disapparition (being able to materialize and
                dematerialize at will)
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="transfiguration"
                name="question17"
                value="transfiguration"
              />
              <label for="transfiguration">
                Transfiguration (turning one object into another object)
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="broomstick"
                name="question17"
                value="broomstick"
              />
              <label for="broomstick">Flying on a broomstick</label>
            </div>
            <div>
              <input type="radio" id="hexes" name="question17" value="hexes" />
              <label for="hexes">Hexes and jinxes</label>
            </div>
            <div>
              <input
                type="radio"
                id="creatures"
                name="question17"
                value="creatures"
              />
              <label for="creatures">
                All about magical creatures, and how to befriend/care for them
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="castle"
                name="question17"
                value="castle"
              />
              <label for="castle">Secrets about the castle</label>
            </div>
            <div>
              <input type="radio" id="area" name="question17" value="area" />
              <label for="area">Every area of magic I can</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank17"
                name="question17"
                value="blank"
              />
              <label for="blank17">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q18:Which of the following would you most like to study?
            </legend>
            <div>
              <input
                type="radio"
                id="centaurs"
                name="question18"
                value="centaurs"
              />
              <label for="centaurs">Centaurs</label>
            </div>
            <div>
              <input
                type="radio"
                id="goblins"
                name="question18"
                value="goblins"
              />
              <label for="goblins">Goblins</label>
            </div>
            <div>
              <input
                type="radio"
                id="merpeople"
                name="question18"
                value="merpeople"
              />
              <label for="merpeople">Merpeople</label>
            </div>
            <div>
              <input
                type="radio"
                id="ghosts"
                name="question18"
                value="ghosts"
              />
              <label for="ghosts">Ghosts</label>
            </div>
            <div>
              <input
                type="radio"
                id="vampires"
                name="question18"
                value="vampires"
              />
              <label for="vampires">Vampires</label>
            </div>
            <div>
              <input
                type="radio"
                id="werewolves"
                name="question18"
                value="werewolves"
              />
              <label for="werewolves">Werewolves</label>
            </div>
            <div>
              <input
                type="radio"
                id="trolls"
                name="question18"
                value="trolls"
              />
              <label for="trolls">Trolls</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank18"
                name="question18"
                value="blank"
              />
              <label for="blank18">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q19:You and two friends need to cross a bridge guarded by a river
              troll who insists on fighting one of you before he will let all of
              you pass. Do you:
            </legend>
            <div>
              <input
                type="radio"
                id="confuse"
                name="question19"
                value="confuse"
              />
              <label for="confuse">
                Attempt to confuse the troll into letting all three of you pass
                without fighting?
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="drawing"
                name="question19"
                value="drawing"
              />
              <label for="drawing">
                Suggest drawing lots to decide which of you will fight?
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="suggest"
                name="question19"
                value="suggest"
              />
              <label for="suggest">
                Suggest that all three of you should fight (without telling the
                troll)?
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="volunteer"
                name="question19"
                value="volunteer"
              />
              <label for="volunteer">Volunteer to fight?</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank19"
                name="question19"
                value="blank"
              />
              <label for="blank19">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q20:One of your house mates has cheated in a Hogwarts exam by
              using a Self-Spelling Quill. Now he has come top of the class in
              Charms, beating you into second place. Professor Flitwick is
              suspicious of what happened. He draws you to one side after his
              lesson and asks you whether or not your classmate used a forbidden
              quill. What do you do?
            </legend>
            <div>
              <input type="radio" id="lie" name="question20" value="lie" />
              <label for="lie">
                Lie and say you don't know (but hope that somebody else tells
                Professor Flitwick the truth)
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="resolve"
                name="question20"
                value="resolve"
              />
              <label for="resolve">
                Tell Professor Flitwick that he ought to ask your classmate (and
                resolve to tell your classmate that if he doesn't tell the
                truth, you will)
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="points"
                name="question20"
                value="points"
              />
              <label for="points">
                Tell Professor Flitwick the truth. If your classmate is prepared
                to win by cheating, he deserves to be found out. Also, as you
                are both in the same house, any points he loses will be regained
                by you, for coming first in his place
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="forbidden"
                name="question20"
                value="forbidden"
              />
              <label for="forbidden">
                You would not wait to be asked to tell Professor Flitwick the
                truth. If you knew that somebody was using a forbidden quill,
                you would tell the teacher before the exam started
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank20"
                name="question20"
                value="blank"
              />
              <label for="blank20">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q21:A Muggle confronts you and says that they are sure you are a
              witch or wizard. Do you:
            </legend>
            <div>
              <input type="radio" id="ask" name="question21" value="ask" />
              <label for="ask">Ask what makes them think so?</label>
            </div>
            <div>
              <input type="radio" id="jinx" name="question21" value="jinx" />
              <label for="jinx">
                Agree, and ask whether they'd like a free sample of a jinx?
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="bluffing"
                name="question21"
                value="bluffing"
              />
              <label for="bluffing">
                Agree, and walk away, leaving them to wonder whether you are
                bluffing?
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="doctor"
                name="question21"
                value="doctor"
              />
              <label for="doctor">
                Tell them that you area worried about their mental health, and
                offer to call a doctor
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank21"
                name="question21"
                value="blank"
              />
              <label for="blank21">Bland</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q22:Which nightmare would frighten you most?</legend>
            <div>
              <input
                type="radio"
                id="standing"
                name="question22"
                value="standing"
              />
              <label for="standing">
                Standing on top of something very high and realizing suddenly
                that there are no hand- or footholds, nor any barrier to stop
                you falling
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="locked"
                name="question22"
                value="locked"
              />
              <label for="locked">
                An eye at the keyhole of the dark, windowless room in which you
                are locked
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="waking"
                name="question22"
                value="waking"
              />
              <label for="waking">
                Waking up to find that neither your friends nor your family have
                any idea who you are
              </label>
            </div>
            <div>
              <input type="radio" id="silly" name="question22" value="silly" />
              <label for="silly">
                Being forced to speak in such a silly voice that hardly anyone
                can understand you, and everyone laughs at you
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank22"
                name="question22"
                value="blank"
              />
              <label for="blank22">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q23:Which road tempts you most?</legend>
            <div>
              <input type="radio" id="lane" name="question23" value="lane" />
              <label for="lane">The wide, sunny, grassy lane</label>
            </div>
            <div>
              <input type="radio" id="alley" name="question23" value="alley" />
              <label for="alley">The narrow, dark, lantern-lit alley</label>
            </div>
            <div>
              <input type="radio" id="woods" name="question23" value="woods" />
              <label for="woods">
                The twisting, leaf-strewn path through woods
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="street"
                name="question23"
                value="street"
              />
              <label for="street">
                The cobbled street lined with ancient buildings
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank23"
                name="question23"
                value="blank"
              />
              <label for="blank23">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q24:Late at night, walking alone down the street, you hear a
              peculiar cry that you believe to have a magical source. Do you:
            </legend>
            <div>
              <input
                type="radio"
                id="caution"
                name="question24"
                value="caution"
              />
              <label for="caution">
                Proceed with caution, keeping one hand on your concealed wand
                and an eye out for any disturbance?
              </label>
            </div>
            <div>
              <input type="radio" id="noise" name="question24" value="noise" />
              <label for="noise">
                Draw your wand and try to discover the source of the noise
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="ground"
                name="question24"
                value="ground"
              />
              <label for="ground">Draw your wand and stand your ground?</label>
            </div>
            <div>
              <input
                type="radio"
                id="shadows"
                name="question24"
                value="shadows"
              />
              <label for="shadows">
                Withdraw into the shadows to await developments, while mentally
                reviewing the most appropriate defensive and offensive spells,
                should trouble occur?
              </label>
            </div>
            <div>
              <input
                type="radio"
                id="blank24"
                name="question24"
                value="blank"
              />
              <label for="blank24">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>
              Q25:If you were attending Hogwarts, which pet would you choose to
              take with you?
            </legend>
            <div>
              <input type="radio" id="tabby" name="question25" value="tabby" />
              <label for="tabby">Tabby cat</label>
            </div>
            <div>
              <input
                type="radio"
                id="siamese"
                name="question25"
                value="siamese"
              />
              <label for="siamese">Siamese cat</label>
            </div>
            <div>
              <input
                type="radio"
                id="ginger"
                name="question25"
                value="ginger"
              />
              <label for="ginger">Ginger cat</label>
            </div>
            <div>
              <input
                type="radio"
                id="blackcat"
                name="question25"
                value="blackcat"
              />
              <label for="blackcat">Black cat</label>
            </div>
            <div>
              <input
                type="radio"
                id="whitecat"
                name="question25"
                value="whitecat"
              />
              <label for="whitecat">White cat</label>
            </div>{" "}
            <div>
              <input type="radio" id="tawny" name="question25" value="tawny" />
              <label for="tawny">Tawny owl</label>
            </div>
            <div>
              <input
                type="radio"
                id="screech"
                name="question25"
                value="screech"
              />
              <label for="screech">Screech owl</label>
            </div>
            <div>
              <input type="radio" id="brown" name="question25" value="brown" />
              <label for="brown">Brown owl</label>
            </div>
            <div>
              <input type="radio" id="snowy" name="question25" value="snowy" />
              <label for="snowy">Snowy owl</label>
            </div>
            <div>
              <input type="radio" id="barn" name="question25" value="barn" />
              <label for="barn">Barn owl</label>
            </div>
            <div>
              <input
                type="radio"
                id="common"
                name="question25"
                value="common"
              />
              <label for="common">Common toad</label>
            </div>{" "}
            <div>
              <input
                type="radio"
                id="natterjack"
                name="question25"
                value="natterjack"
              />
              <label for="natterjack">Natterjack toad</label>
            </div>
            <div>
              <input
                type="radio"
                id="dragon"
                name="question25"
                value="dragon"
              />
              <label for="dragon">Dragon toad</label>
            </div>
            <div>
              <input
                type="radio"
                id="harlequin"
                name="question25"
                value="harlequin"
              />
              <label for="harlequin">Harlequin toad</label>
            </div>
            <div>
              <input type="radio" id="three" name="question25" value="three" />
              <label for="three">Three toed tree toad</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank25"
                name="question25"
                value="blank"
              />
              <label for="blank25">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q26:Black or white?</legend>
            <div>
              <input type="radio" id="black" name="question26" value="black" />
              <label for="black">Black</label>
            </div>
            <div>
              <input type="radio" id="white" name="question26" value="white" />
              <label for="white">White</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank26"
                name="question26"
                value="blank"
              />
              <label for="blank26">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q27:Heads or tails?</legend>
            <div>
              <input type="radio" id="heads" name="question27" value="heads" />
              <label for="heads">Heads</label>
            </div>
            <div>
              <input type="radio" id="tails" name="question27" value="tails" />
              <label for="tails">Tails</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank27"
                name="question27"
                value="blank"
              />
              <label for="blank27">Blank</label>
            </div>
          </fieldset>
          <fieldset>
            <legend>Q28:Left or right?</legend>
            <div>
              <input type="radio" id="left" name="question28" value="left" />
              <label for="left">Left</label>
            </div>
            <div>
              <input type="radio" id="right" name="question28" value="right" />
              <label for="right">Right</label>
            </div>
            <div>
              <input
                type="radio"
                id="blank28"
                name="question28"
                value="blank"
              />
              <label for="blank28">Blank</label>
            </div>
          </fieldset>
          <div>
            <input type="submit" value="Submit" />
            <input type="reset" value="Reset" />
          </div>
        </form>

        <figure>
          <img
            id="gryffindor"
            src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylhvu-0c830b7e-4f22-4967-bc36-9c84ad4153af.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_gryffindor_traits_wallpaper_by_emily_corene_d7ylhvu-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsaHZ1LTBjODMwYjdlLTRmMjItNDk2Ny1iYzM2LTljODRhZDQxNTNhZi5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.hBI_8rRTzEUjiWVx6uYQFUhwnlmEBuNSSld0kYF5xMc"
            width="100%"
            alt="griffindor house"
            class="image-fluid"
            hidden
          />
        </figure>

        <figure>
          <img
            id="ravenclaw"
            src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylh6e-fc754741-72d1-4b9f-a498-dad48b0a0cf2.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_ravenclaw_traits_wallpaper_by_emily_corene_d7ylh6e-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsaDZlLWZjNzU0NzQxLTcyZDEtNGI5Zi1hNDk4LWRhZDQ4YjBhMGNmMi5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.oqHqAuCwHIdlSt-2IaNbUcvvnk8VMNEHhzB1bgkOMbU"
            width="100%"
            alt="ravenclaw house"
            class="image-fluid"
            hidden
          />
        </figure>

        <figure>
          <img
            id="hufflepuff"
            src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylice-34e3ca1b-5494-4b0e-9c85-7588d91173f9.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_hufflepuff_traits_wallpaper_by_emily_corene_d7ylice-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsaWNlLTM0ZTNjYTFiLTU0OTQtNGIwZS05Yzg1LTc1ODhkOTExNzNmOS5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.mOGcMxt82sDS9I3zwPkwc9JXaEWiWksEaERrIBnEHjY"
            width="100%"
            alt="hufflepuff house"
            class="image-fluid"
            hidden
          />
        </figure>

        <figure>
          <img
            id="slytherin"
            src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylja7-5f9eb0e3-9619-4783-9955-407bd02359b7.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_slytherin_traits_wallpaper_by_emily_corene_d7ylja7-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsamE3LTVmOWViMGUzLTk2MTktNDc4My05OTU1LTQwN2JkMDIzNTliNy5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.BeGzGnOX_v4UUlvvAn_4G0DT8xbtx64ZuS9-IQx3_10"
            width="100%"
            alt="slytherin house"
            class="image-fluid"
            hidden
          />
        </figure>

        {/*A gif of seamus in the event that the user enters nothing and clicks submit*/}
        <figure>
          <img
            id="noanswer"
            src="https://media.giphy.com/media/U5Dg7awkAwBDmNbWzp/giphy.gif"
            width="100%"
            alt="seamus gif"
            class="image-fluid"
            hidden
          />
        </figure>
    </div>
  );
}
