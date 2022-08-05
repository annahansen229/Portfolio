import "bootstrap/dist/css/bootstrap.min.css";
import "../App.css";
import Image from "../images/hedwig.gifv";
import Image2 from "../images/boats.gif";
import Image3 from "../images/seamus.gif";
import Image4 from "../images/gryffindor.jpg";
import Image5 from "../images/ravenclaw.jpg";
import Image6 from "../images/hufflepuff.jpg";
import Image7 from "../images/slytherin.jpg";
import Image8 from "../images/hagrid.gif";
import Image9 from "../images/harry-wizard.gif"
import Audio from "../images/hedwigs_theme.mp3"

import Navbar from "../components/navbar.js";

export default function Home() {
  //For the sorting hat quiz on the homepage
  //all houses start with 0 points and all images hidden including the gif
  //variables for the Hogwarts Houses
  let gryffindor = 0;
  let ravenclaw = 0;
  let hufflepuff = 0;
  let slytherin = 0;
  function checkValue() {
    // function to calculate random int in case of a tiebreak
    function getRandomInt(max) {
      return Math.floor(Math.random() * max);
    }
    console.group("========= Form Submission =========");
    let form = document.querySelector("form");
    if (form) {
      form.addEventListener("submit", (event) => {
        //Submissions for question1
        //note any answer with blank will not award any point to any house
        console.log(form.elements.question1.value);
        if (form.elements.question1.value === "dawn") {
          console.log("Dawn was chosen");
          gryffindor++; //add 1 point to both gryffindor and ravenclaw
          ravenclaw++;
          console.log(gryffindor);
          console.log(ravenclaw);
        } else if (form.elements.question1.value === "dusk") {
          console.log("Dusk was chosen");
          hufflepuff++; //add 1 point to both hufflepuff and slytherin
          slytherin++;
        }
        //submissions for question2
        if (form.elements.question2.value === "forest") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question2.value === "river") {
          hufflepuff++;
          slytherin++;
        }

        //submissions for question3
        if (form.elements.question3.value === "moon") {
          ravenclaw++;
          slytherin++;
        } else if (form.elements.question3.value === "stars") {
          gryffindor++;
          hufflepuff++;
        }

        //submissions for question6
        if (form.elements.question6.value === "wise") {
          ravenclaw++;
        } else if (form.elements.question6.value === "good") {
          hufflepuff++;
        } else if (form.elements.question6.value === "great") {
          slytherin++;
        } else if (form.elements.question6.value === "bold") {
          gryffindor++;
        }

        //submissions for question7
        if (form.elements.question7.value === "love") {
          hufflepuff++;
        } else if (form.elements.question7.value === "glory") {
          gryffindor++;
        } else if (form.elements.question7.value === "wisdom") {
          ravenclaw++;
        } else if (form.elements.question7.value === "power") {
          slytherin++;
        }

        //submissions for question11
        if (form.elements.question11.value === "apples") {
          ravenclaw++;
        } else if (form.elements.question11.value === "toadstools") {
          hufflepuff++;
        } else if (form.elements.question11.value === "pool") {
          slytherin++;
        } else if (form.elements.question11.value === "statue") {
          gryffindor++;
        }

        //submissions for question15
        if (form.elements.question15.value === "envied") {
          ravenclaw++;
          slytherin++;
        } else if (form.elements.question15.value === "imitated") {
          ravenclaw++;
        } else if (form.elements.question15.value === "trusted") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question15.value === "praised") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question15.value === "liked") {
          hufflepuff++;
        } else if (form.elements.question15.value === "feared") {
          slytherin++;
        }

        //submissions for question16
        if (form.elements.question16.value === "minds") {
          ravenclaw++;
          slytherin++;
        } else if (form.elements.question16.value === "invisibility") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question16.value === "strength") {
          hufflepuff++;
          slytherin++;
        } else if (form.elements.question16.value === "animals") {
          ravenclaw++;
          hufflepuff++;
        } else if (form.elements.question16.value === "past") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question16.value === "appearance") {
          gryffindor++;
          ravenclaw++;
        }

        //submissions for question17
        if (form.elements.question17.value === "apparition") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question17.value === "transfiguration") {
          ravenclaw++;
        } else if (form.elements.question17.value === "broomstick") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question17.value === "hexes") {
          slytherin++;
        } else if (form.elements.question17.value === "creatures") {
          hufflepuff++;
        } else if (form.elements.question17.value === "castle") {
          gryffindor++;
        } else if (form.elements.question17.value === "area") {
          ravenclaw++;
        }

        //submissions for question18
        if (form.elements.question18.value === "centaurs") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question18.value === "goblins") {
          ravenclaw++;
          slytherin++;
        } else if (form.elements.question18.value === "merpeople") {
          hufflepuff++;
          slytherin++;
        } else if (form.elements.question18.value === "ghosts") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question18.value === "vampires") {
          slytherin++;
        } else if (form.elements.question18.value === "werewolves") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question18.value === "trolls") {
          hufflepuff++;
          slytherin++;
        }

        //submissions for question19
        if (form.elements.question19.value === "confuse") {
          ravenclaw++;
        } else if (form.elements.question19.value === "drawing") {
          hufflepuff++;
        } else if (form.elements.question19.value === "suggest") {
          slytherin++;
        } else if (form.elements.question19.value === "volunteer") {
          gryffindor++;
        }

        //submissions for question25
        if (form.elements.question25.value === "tabby") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question25.value === "siamese") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question25.value === "ginger") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question25.value === "blackcat") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question25.value === "whitecat") {
          gryffindor++;
          slytherin++;
        } else if (form.elements.question25.value === "tawny") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question25.value === "screech") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question25.value === "brown") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question25.value === "snowy") {
          gryffindor++;
          ravenclaw++;
          hufflepuff++;
        } else if (form.elements.question25.value === "barn") {
          gryffindor++;
          ravenclaw++;
        } else if (form.elements.question25.value === "common") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question25.value === "natterjack") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question25.value === "dragon") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question25.value === "harlequin") {
          gryffindor++;
          hufflepuff++;
        } else if (form.elements.question25.value === "three") {
          gryffindor++;
          ravenclaw++;
          hufflepuff++;
        }

        //now to tally the results! First if one house has the most points
        //then the sorting hat will choose that house for the user
        if (
          gryffindor > ravenclaw &&
          gryffindor > hufflepuff &&
          gryffindor > slytherin
        ) {
          document.getElementById("gryffindor").removeAttribute("hidden");
          document.body.style.backgroundColor = "#e2062c";
          document.getElementById("noanswer").setAttribute("hidden", "hidden");
        } else if (
          ravenclaw > gryffindor &&
          ravenclaw > hufflepuff &&
          ravenclaw > slytherin
        ) {
          document.getElementById("ravenclaw").removeAttribute("hidden");
          document.body.style.backgroundColor = "blue";
          document.getElementById("noanswer").setAttribute("hidden", "hidden");
        } else if (
          hufflepuff > ravenclaw &&
          hufflepuff > gryffindor &&
          hufflepuff > slytherin
        ) {
          document.getElementById("hufflepuff").removeAttribute("hidden");
          document.body.style.backgroundColor = "yellow";
          document.getElementById("noanswer").setAttribute("hidden", "hidden");
        } else if (
          slytherin > gryffindor &&
          slytherin > ravenclaw &&
          slytherin > hufflepuff
        ) {
          document.getElementById("slytherin").removeAttribute("hidden");
          document.body.style.backgroundColor = "green";
          document.getElementById("noanswer").setAttribute("hidden", "hidden");
          //in the event that nothing is entered then seamus gif will appear!
        } else if (
          gryffindor === 0 &&
          ravenclaw === 0 &&
          hufflepuff === 0 &&
          slytherin === 0
        ) {
          document.getElementById("noanswer").removeAttribute("hidden");
        } else {
          //in the event of a tie then random number function above will be used
          let tiebreak = getRandomInt(4); //generate number between 0 and 3
          gryffindor = 0;
          ravenclaw = 1;
          hufflepuff = 2;
          slytherin = 3;
          if (tiebreak === 0) {
            document.getElementById("gryffindor").removeAttribute("hidden");
            document
              .getElementById("noanswer")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("ravenclaw")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("hufflepuff")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("slytherin")
              .setAttribute("hidden", "hidden");
            document.body.style.backgroundColor = "#e2062c";
          } else if (tiebreak === 1) {
            document.getElementById("ravenclaw").removeAttribute("hidden");
            document
              .getElementById("noanswer")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("gryffindor")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("hufflepuff")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("slytherin")
              .setAttribute("hidden", "hidden");
            document.body.style.backgroundColor = "blue";
          } else if (tiebreak === 2) {
            document.getElementById("hufflepuff").removeAttribute("hidden");
            document
              .getElementById("noanswer")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("gryffindor")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("ravenclaw")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("slytherin")
              .setAttribute("hidden", "hidden");
            document.body.style.backgroundColor = "yellow";
          } else if (tiebreak === 3) {
            document.getElementById("slytherin").removeAttribute("hidden");
            document
              .getElementById("noanswer")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("gryffindor")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("ravenclaw")
              .setAttribute("hidden", "hidden");
            document
              .getElementById("hufflepuff")
              .setAttribute("hidden", "hidden");
            document.body.style.backgroundColor = "green";
          }
        }
        console.log(gryffindor);
        console.log(ravenclaw);
        console.log(hufflepuff);
        console.log(slytherin);

        event.preventDefault();
      });
    }

    //adding event listener for when user hits the reset button
    if (form) {
      form.addEventListener("reset", (event) => {
        //reset the values for the houses and reset the images
        gryffindor = 0;
        ravenclaw = 0;
        hufflepuff = 0;
        slytherin = 0;
        document.getElementById("gryffindor").setAttribute("hidden", "hidden");
        document.getElementById("ravenclaw").setAttribute("hidden", "hidden");
        document.getElementById("hufflepuff").setAttribute("hidden", "hidden");
        document.getElementById("slytherin").setAttribute("hidden", "hidden");
        document.getElementById("noanswer").setAttribute("hidden", "hidden");
        document.body.style.backgroundColor = "white";
      });
    }
  }

  checkValue();

  return (
    <div className="Home">
      <Navbar />
      <h1 className="frontpage">The adventure begins...</h1>
      <audio
        controls
        autoPlay
        src={Audio}
        type="audio/mpeg"
        alt="Hedwig's Theme"
      ></audio>
      <figure>
        <img
          src="https://media.tumblr.com/tumblr_m9laumf6Wi1royzou.gif"
          width="100%"
          alt="hogwarts letter gif"
        />
        <figcaption>Hogwarts Letter</figcaption>
      </figure>

      <figure>
        <img src={Image8} width="100%" alt="hagrid gif" />
        <figcaption>Hagrid</figcaption>
      </figure>
      <figure>
        <img src={Image9} width="100%" alt="wizard gif" />
        <figcaption>Wizard</figcaption>
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
      <h2 className="quiztime"> Now it is your turn! </h2>
      <form
        className="sortinghat"
        action=""
        onClick={checkValue}
        onSubmit={checkValue}
      >
        <fieldset>
          <legend className="legend">Sorting Hat Quiz</legend>
          <legend>Q1: Dawn or Dusk?</legend>
          <div>
            <input type="radio" id="dawn" name="question1" value="dawn" />
            <label htmlFor="dawn">Dawn</label>
          </div>
          <div>
            <input type="radio" id="dusk" name="question1" value="dusk" />
            <label htmlFor="dusk">Dusk</label>
          </div>
          <div>
            <input type="radio" id="blank1" name="question1" value="blank" />
            <label htmlFor="blank1">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>Q2: Forest or River?</legend>
          <div>
            <input type="radio" id="forest" name="question2" value="forest" />
            <label htmlFor="forest">Forest</label>
          </div>
          <div>
            <input type="radio" id="river" name="question2" value="river" />
            <label htmlFor="river">River</label>
          </div>
          <div>
            <input type="radio" id="blank2" name="question2" value="blank" />
            <label htmlFor="blank2">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>Q3:Moon or Stars?</legend>
          <div>
            <input type="radio" id="moon" name="question3" value="moon" />
            <label htmlFor="moon">Moon</label>
          </div>
          <div>
            <input type="radio" id="stars" name="question3" value="stars" />
            <label htmlFor="stars">Stars</label>
          </div>
          <div>
            <input type="radio" id="blank3" name="question3" value="blank" />
            <label htmlFor="blank3">Blank</label>
          </div>
        </fieldset>

        <fieldset>
          <legend>Q4:How would you like to be known to history?</legend>
          <div>
            <input type="radio" id="wise" name="question6" value="wise" />
            <label htmlFor="wise">The Wise</label>
          </div>
          <div>
            <input type="radio" id="good" name="question6" value="good" />
            <label htmlFor="good">The Good</label>
          </div>
          <div>
            <input type="radio" id="great" name="question6" value="great" />
            <label htmlFor="great">The Great</label>
          </div>
          <div>
            <input type="radio" id="bold" name="question6" value="bold" />
            <label htmlFor="bold">The Bold</label>
          </div>
          <div>
            <input type="radio" id="blank6" name="question6" value="blank" />
            <label htmlFor="blank6">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>
            Q5:Given the choice, would you rather invent a potion that would
            guarantee you:
          </legend>
          <div>
            <input type="radio" id="love" name="question7" value="love" />
            <label htmlFor="love">Love?</label>
          </div>
          <div>
            <input type="radio" id="glory" name="question7" value="glory" />
            <label htmlFor="glory">Glory?</label>
          </div>
          <div>
            <input type="radio" id="wisdom" name="question7" value="wisdom" />
            <label htmlFor="wisdom">Wisdom?</label>
          </div>
          <div>
            <input type="radio" id="power" name="question7" value="power" />
            <label htmlFor="power">Power?</label>
          </div>
          <div>
            <input type="radio" id="blank7" name="question7" value="blank" />
            <label htmlFor="blank7">Blank</label>
          </div>
        </fieldset>

        <fieldset>
          <legend>
            Q6:You enter an enchanted garden. What would you be most curious to
            examine first?
          </legend>
          <div>
            <input type="radio" id="apples" name="question11" value="apples" />
            <label htmlFor="apples">
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
            <label htmlFor="toadstools">
              The fat red toadstools that appear to be talking to each other
            </label>
          </div>
          <div>
            <input type="radio" id="pool" name="question11" value="pool" />
            <label htmlFor="pool">
              The bubbling pool, in the depths of which something luminous is
              swirling
            </label>
          </div>
          <div>
            <input type="radio" id="statue" name="question11" value="statue" />
            <label htmlFor="statue">
              The statue of an old wizard with a strangely twinkling eye
            </label>
          </div>
          <div>
            <input type="radio" id="blank11" name="question11" value="blank" />
            <label htmlFor="blank11">Blank</label>
          </div>
        </fieldset>

        <fieldset>
          <legend>Q7:Which would you rather be:</legend>
          <div>
            <input type="radio" id="envied" name="question15" value="envied" />
            <label htmlFor="envied">Envied?</label>
          </div>
          <div>
            <input
              type="radio"
              id="imitated"
              name="question15"
              value="imitated"
            />
            <label htmlFor="imitated">Imitated?</label>
          </div>
          <div>
            <input
              type="radio"
              id="trusted"
              name="question15"
              value="trusted"
            />
            <label htmlFor="trusted">Trusted?</label>
          </div>
          <div>
            <input
              type="radio"
              id="praised"
              name="question15"
              value="praised"
            />
            <label htmlFor="praised">Praised?</label>
          </div>
          <div>
            <input type="radio" id="liked" name="question15" value="liked" />
            <label htmlFor="liked">Liked?</label>
          </div>
          <div>
            <input type="radio" id="feared" name="question15" value="feared" />
            <label htmlFor="feared">Feared?</label>
          </div>
          <div>
            <input type="radio" id="blank15" name="question15" value="blank" />
            <label htmlFor="blank15">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>
            Q8:If you could have any power, which would you choose?
          </legend>
          <div>
            <input type="radio" id="minds" name="question16" value="minds" />
            <label htmlFor="minds">The power to read minds</label>
          </div>
          <div>
            <input
              type="radio"
              id="invisibility"
              name="question16"
              value="invisibility"
            />
            <label htmlFor="invisibility">The power of invisibility</label>
          </div>
          <div>
            <input
              type="radio"
              id="strength"
              name="question16"
              value="strength"
            />
            <label htmlFor="strength">The power of superhuman strength</label>
          </div>
          <div>
            <input
              type="radio"
              id="animals"
              name="question16"
              value="animals"
            />
            <label htmlFor="animals">The power to speak to animals</label>
          </div>
          <div>
            <input type="radio" id="past" name="question16" value="past" />
            <label htmlFor="past">The power to change the past</label>
          </div>
          <div>
            <input
              type="radio"
              id="appearance"
              name="question16"
              value="appearance"
            />
            <label htmlFor="appearance">
              The power to change your appearance at will
            </label>
          </div>
          <div>
            <input type="radio" id="blank16" name="question16" value="blank" />
            <label htmlFor="blank16">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>
            Q9:What are you most looking forward to learning at Hogwarts?
          </legend>
          <div>
            <input
              type="radio"
              id="apparition"
              name="question17"
              value="apparition"
            />
            <label htmlFor="apparition">
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
            <label htmlFor="transfiguration">
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
            <label htmlFor="broomstick">Flying on a broomstick</label>
          </div>
          <div>
            <input type="radio" id="hexes" name="question17" value="hexes" />
            <label htmlFor="hexes">Hexes and jinxes</label>
          </div>
          <div>
            <input
              type="radio"
              id="creatures"
              name="question17"
              value="creatures"
            />
            <label htmlFor="creatures">
              All about magical creatures, and how to befriend/care for them
            </label>
          </div>
          <div>
            <input type="radio" id="castle" name="question17" value="castle" />
            <label htmlFor="castle">Secrets about the castle</label>
          </div>
          <div>
            <input type="radio" id="area" name="question17" value="area" />
            <label htmlFor="area">Every area of magic I can</label>
          </div>
          <div>
            <input type="radio" id="blank17" name="question17" value="blank" />
            <label htmlFor="blank17">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>
            Q10:Which of the following would you most like to study?
          </legend>
          <div>
            <input
              type="radio"
              id="centaurs"
              name="question18"
              value="centaurs"
            />
            <label htmlFor="centaurs">Centaurs</label>
          </div>
          <div>
            <input
              type="radio"
              id="goblins"
              name="question18"
              value="goblins"
            />
            <label htmlFor="goblins">Goblins</label>
          </div>
          <div>
            <input
              type="radio"
              id="merpeople"
              name="question18"
              value="merpeople"
            />
            <label htmlFor="merpeople">Merpeople</label>
          </div>
          <div>
            <input type="radio" id="ghosts" name="question18" value="ghosts" />
            <label htmlFor="ghosts">Ghosts</label>
          </div>
          <div>
            <input
              type="radio"
              id="vampires"
              name="question18"
              value="vampires"
            />
            <label htmlFor="vampires">Vampires</label>
          </div>
          <div>
            <input
              type="radio"
              id="werewolves"
              name="question18"
              value="werewolves"
            />
            <label htmlFor="werewolves">Werewolves</label>
          </div>
          <div>
            <input type="radio" id="trolls" name="question18" value="trolls" />
            <label htmlFor="trolls">Trolls</label>
          </div>
          <div>
            <input type="radio" id="blank18" name="question18" value="blank" />
            <label htmlFor="blank18">Blank</label>
          </div>
        </fieldset>
        <fieldset>
          <legend>
            Q11:You and two friends need to cross a bridge guarded by a river
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
            <label htmlFor="confuse">
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
            <label htmlFor="drawing">
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
            <label htmlFor="suggest">
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
            <label htmlFor="volunteer">Volunteer to fight?</label>
          </div>
          <div>
            <input type="radio" id="blank19" name="question19" value="blank" />
            <label htmlFor="blank19">Blank</label>
          </div>
        </fieldset>

        <fieldset>
          <legend>
            Q12:If you were attending Hogwarts, which pet would you choose to
            take with you?
          </legend>
          <div>
            <input type="radio" id="tabby" name="question25" value="tabby" />
            <label htmlFor="tabby">Tabby cat</label>
          </div>
          <div>
            <input
              type="radio"
              id="siamese"
              name="question25"
              value="siamese"
            />
            <label htmlFor="siamese">Siamese cat</label>
          </div>
          <div>
            <input type="radio" id="ginger" name="question25" value="ginger" />
            <label htmlFor="ginger">Ginger cat</label>
          </div>
          <div>
            <input
              type="radio"
              id="blackcat"
              name="question25"
              value="blackcat"
            />
            <label htmlFor="blackcat">Black cat</label>
          </div>
          <div>
            <input
              type="radio"
              id="whitecat"
              name="question25"
              value="whitecat"
            />
            <label htmlFor="whitecat">White cat</label>
          </div>{" "}
          <div>
            <input type="radio" id="tawny" name="question25" value="tawny" />
            <label htmlFor="tawny">Tawny owl</label>
          </div>
          <div>
            <input
              type="radio"
              id="screech"
              name="question25"
              value="screech"
            />
            <label htmlFor="screech">Screech owl</label>
          </div>
          <div>
            <input type="radio" id="brown" name="question25" value="brown" />
            <label htmlFor="brown">Brown owl</label>
          </div>
          <div>
            <input type="radio" id="snowy" name="question25" value="snowy" />
            <label htmlFor="snowy">Snowy owl</label>
          </div>
          <div>
            <input type="radio" id="barn" name="question25" value="barn" />
            <label htmlFor="barn">Barn owl</label>
          </div>
          <div>
            <input type="radio" id="common" name="question25" value="common" />
            <label htmlFor="common">Common toad</label>
          </div>{" "}
          <div>
            <input
              type="radio"
              id="natterjack"
              name="question25"
              value="natterjack"
            />
            <label htmlFor="natterjack">Natterjack toad</label>
          </div>
          <div>
            <input type="radio" id="dragon" name="question25" value="dragon" />
            <label htmlFor="dragon">Dragon toad</label>
          </div>
          <div>
            <input
              type="radio"
              id="harlequin"
              name="question25"
              value="harlequin"
            />
            <label htmlFor="harlequin">Harlequin toad</label>
          </div>
          <div>
            <input type="radio" id="three" name="question25" value="three" />
            <label htmlFor="three">Three toed tree toad</label>
          </div>
          <div>
            <input type="radio" id="blank25" name="question25" value="blank" />
            <label htmlFor="blank25">Blank</label>
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
          //src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylhvu-0c830b7e-4f22-4967-bc36-9c84ad4153af.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_gryffindor_traits_wallpaper_by_emily_corene_d7ylhvu-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsaHZ1LTBjODMwYjdlLTRmMjItNDk2Ny1iYzM2LTljODRhZDQxNTNhZi5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.hBI_8rRTzEUjiWVx6uYQFUhwnlmEBuNSSld0kYF5xMc"
          src={Image4}
          width="100%"
          alt="griffindor house"
          className="image-fluid"
          hidden
        />
      </figure>

      <figure>
        <img
          id="ravenclaw"
          //src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylh6e-fc754741-72d1-4b9f-a498-dad48b0a0cf2.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_ravenclaw_traits_wallpaper_by_emily_corene_d7ylh6e-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsaDZlLWZjNzU0NzQxLTcyZDEtNGI5Zi1hNDk4LWRhZDQ4YjBhMGNmMi5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.oqHqAuCwHIdlSt-2IaNbUcvvnk8VMNEHhzB1bgkOMbU"
          src={Image5}
          width="100%"
          alt="ravenclaw house"
          className="image-fluid"
          hidden
        />
      </figure>

      <figure>
        <img
          id="hufflepuff"
          //src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylice-34e3ca1b-5494-4b0e-9c85-7588d91173f9.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_hufflepuff_traits_wallpaper_by_emily_corene_d7ylice-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsaWNlLTM0ZTNjYTFiLTU0OTQtNGIwZS05Yzg1LTc1ODhkOTExNzNmOS5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.mOGcMxt82sDS9I3zwPkwc9JXaEWiWksEaERrIBnEHjY"
          src={Image6}
          width="100%"
          alt="hufflepuff house"
          className="image-fluid"
          hidden
        />
      </figure>

      <figure>
        <img
          id="slytherin"
          //src="https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/6cfdc50d-9688-4b0b-95eb-c7d1411f6403/d7ylja7-5f9eb0e3-9619-4783-9955-407bd02359b7.jpg/v1/fill/w_1150,h_695,q_70,strp/hd_slytherin_traits_wallpaper_by_emily_corene_d7ylja7-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9OTY3IiwicGF0aCI6IlwvZlwvNmNmZGM1MGQtOTY4OC00YjBiLTk1ZWItYzdkMTQxMWY2NDAzXC9kN3lsamE3LTVmOWViMGUzLTk2MTktNDc4My05OTU1LTQwN2JkMDIzNTliNy5qcGciLCJ3aWR0aCI6Ijw9MTYwMCJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.BeGzGnOX_v4UUlvvAn_4G0DT8xbtx64ZuS9-IQx3_10"
          src={Image7}
          width="100%"
          alt="slytherin house"
          className="image-fluid"
          hidden
        />
      </figure>

      {/*A gif of seamus in the event that the user enters nothing and clicks submit*/}
      <figure>
        <img
          id="noanswer"
          //src="https://media.giphy.com/media/U5Dg7awkAwBDmNbWzp/giphy.gif"
          src={Image3}
          width="100%"
          alt="seamus gif"
          className="image-fluid"
          hidden
        />
      </figure>
    </div>
  );
}
