//For the sorting hat quiz on the homepage
//all houses start with 0 points and all images hidden including the gif
//variables for the Hogwarts Houses
let gryffindor, ravenclaw, hufflepuff, slytherin = 0;

  //document.getElementById("gryffindor").style.visibility = 'hidden';
  //document.getElementById("ravenclaw").style.visibility = 'hidden';
  //document.getElementById("hufflepuff").style.visibility = 'hidden';
 // document.getElementById("slytherin").style.visibility = 'hidden';
 // document.getElementById("noanswer").style.visibility = 'hidden';

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

  //submissions for question4
  if (form.elements.question4.value === "ordinary") {
    slytherin++;
  } else if (form.elements.question4.value === "ignorant") {
    ravenclaw++;
  } else if (form.elements.question4.value === "cowardly") {
    gryffindor++;
  } else if (form.elements.question4.value === "selfish") {
    hufflepuff++;
  }

  //submissions for question5
  if (form.elements.question5.value === "miss") {
    hufflepuff++;
  } else if (form.elements.question5.value === "stories") {
    gryffindor++;
  } else if (form.elements.question5.value === "think") {
    ravenclaw++;
  } else if (form.elements.question5.value === "i") {
    slytherin++;
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

  //submissions for question8
  if (form.elements.question8.value === "fire") {
    gryffindor++;
  } else if (form.elements.question8.value === "sea") {
    slytherin++;
  } else if (form.elements.question8.value === "parchment") {
    ravenclaw++;
  } else if (form.elements.question8.value === "home") {
    hufflepuff++;
  }

  //submissions for question9
  if (form.elements.question9.value === "diamonds") {
    ravenclaw++;
  } else if (form.elements.question9.value === "plums") {
    hufflepuff++;
  } else if (form.elements.question9.value === "sunspots") {
    gryffindor++;
  } else if (form.elements.question9.value === "ink") {
    slytherin++;
  }

  //submissions for question10
  if (form.elements.question10.value === "violin") {
    slytherin++;
  } else if (form.elements.question10.value === "trumpet") {
    hufflepuff++;
  } else if (form.elements.question10.value === "piano") {
    ravenclaw++;
  } else if (form.elements.question10.value === "drum") {
    gryffindor++;
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

  //submissions for question12
  if (form.elements.question12.value === "tortoiseshell") {
    hufflepuff++;
  } else if (form.elements.question12.value === "rune") {
    slytherin++;
  } else if (form.elements.question12.value === "ornate") {
    ravenclaw++;
  } else if (form.elements.question12.value === "pewter") {
    gryffindor++;
  }

  //submissions for question13
  if (form.elements.question13.value === "crb") {
    gryffindor++;
    hufflepuff++;
  } else if (form.elements.question13.value === "rbc") {
    slytherin++;
  } else if (form.elements.question13.value === "bcr") {
    ravenclaw++;
  } else if (form.elements.question13.value === "cbr") {
    gryffindor++;
  } else if (form.elements.question13.value === "rcb") {
    ravenclaw++;
  } else if (form.elements.question13.value === "brc") {
    ravenclaw++;
    slytherin++;
  }

  //submissions for question14
  if (form.elements.question14.value === "hunger") {
    ravenclaw++;
    hufflepuff++;
  } else if (form.elements.question14.value === "cold") {
    hufflepuff++;
    slytherin++;
  } else if (form.elements.question14.value === "loneliness") {
    gryffindor++;
    hufflepuff++;
  } else if (form.elements.question14.value === "boredom") {
    gryffindor++;
    slytherin++;
  } else if (form.elements.question14.value === "ignored") {
    ravenclaw++;
    slytherin++;
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

  //submissions for question20
  if (form.elements.question20.value === "lie") {
    hufflepuff++;
  } else if (form.elements.question20.value === "resolve") {
    gryffindor++;
  } else if (form.elements.question20.value === "points") {
    ravenclaw++;
  } else if (form.elements.question20.value === "forbidden") {
    slytherin++;
  }

  //submissions for question21
  if (form.elements.question21.value === "ask") {
    ravenclaw++;
  } else if (form.elements.question21.value === "jinx") {
    slytherin++;
  } else if (form.elements.question21.value === "bluffing") {
    gryffindor++;
  } else if (form.elements.question21.value === "doctor") {
    hufflepuff++;
  }
  //submissions for question22
  if (form.elements.question22.value === "standing") {
    ravenclaw++;
  } else if (form.elements.question22.value === "locked") {
    gryffindor++;
  } else if (form.elements.question22.value === "waking") {
    hufflepuff++;
  } else if (form.elements.question22.value === "silly") {
    slytherin++;
  }

  //submissions for question23
  if (form.elements.question23.value === "lane") {
    hufflepuff++;
  } else if (form.elements.question23.value === "alley") {
    slytherin++;
  } else if (form.elements.question23.value === "woods") {
    gryffindor++;
  } else if (form.elements.question23.value === "street") {
    ravenclaw++;
  }

  //submissions for question24
  if (form.elements.question24.value === "caution") {
    hufflepuff++;
  } else if (form.elements.question24.value === "noise") {
    gryffindor++;
  } else if (form.elements.question24.value === "ground") {
    slytherin++;
  } else if (form.elements.question24.value === "shadows") {
    ravenclaw++;
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

  //submissions for question26
  if (form.elements.question26.value === "black") {
    gryffindor++;
    slytherin++;
  } else if (form.elements.question26.value === "white") {
    ravenclaw++;
    hufflepuff++;
  }

  //submissions for question27
  if (form.elements.question27.value === "heads") {
    ravenclaw++;
    hufflepuff++;
  } else if (form.elements.question27.value === "tails") {
    gryffindor++;
    slytherin++;
  }

  //submissions for question28
  if (form.elements.question28.value === "left") {
    ravenclaw++;
    slytherin++;
  } else if (form.elements.question28.value === "right") {
    gryffindor++;
    hufflepuff++;
  }

  //now to tally the results! First if one house has the most points
  //then the sorting hat will choose that house for the user
  if (
    gryffindor > ravenclaw &&
    gryffindor > hufflepuff &&
    gryffindor > slytherin
  ) {
    document.getElementById("gryffindor").removeAttribute('hidden');
    document.getElementById("noanswer").setAttribute('hidden', 'hidden');
  } else if (
    ravenclaw > gryffindor &&
    ravenclaw > hufflepuff &&
    ravenclaw > slytherin
  ) {
    document.getElementById("ravenclaw").removeAttribute('hidden');
    document.getElementById("noanswer").setAttribute('hidden', 'hidden');
  } else if (
    hufflepuff > ravenclaw &&
    hufflepuff > gryffindor &&
    hufflepuff > slytherin
  ) {
    document.getElementById("hufflepuff").removeAttribute('hidden');
    document.getElementById("noanswer").setAttribute('hidden', 'hidden');
  } else if (
    slytherin > gryffindor &&
    slytherin > ravenclaw &&
    slytherin > hufflepuff
  ) {
    document.getElementById("slytherin").removeAttribute('hidden');
    document.getElementById("noanswer").setAttribute('hidden', 'hidden');
    //in the event that nothing is entered then seamus gif will appear!
  } else if (
    gryffindor === 0 &&
    ravenclaw === 0 &&
    hufflepuff === 0 &&
    slytherin === 0
  ) {
    document.getElementById("noanswer").removeAttribute('hidden');
  } else {
    //in the event of a tie then random number function above will be used
    let tiebreak = getRandomInt(4); //generate number between 0 and 3
    gryffindor = 0;
    ravenclaw = 1;
    hufflepuff = 2;
    slytherin = 3;
    if (tiebreak === 0) {
      document.getElementById("gryffindor").removeAttribute('hidden');
    //  document.getElementById("noanswer").setAttribute('hidden', 'hidden');
    //  document.getElementById("ravenclaw").setAttribute('hidden', 'hidden');
    //  document.getElementById("hufflepuff").setAttribute('hidden', 'hidden');
    //  document.getElementById("slytherin").setAttribute('hidden', 'hidden');
      document.body.style.backgroundColor = "#e2062c";
    } else if (tiebreak === 1) {
      document.getElementById("ravenclaw").removeAttribute('hidden');
    //  document.getElementById("noanswer").setAttribute('hidden', 'hidden');
    //  document.getElementById("gryffindor").setAttribute('hidden', 'hidden');
    //  document.getElementById("hufflepuff").setAttribute('hidden', 'hidden');
    //  document.getElementById("slytherin").setAttribute('hidden', 'hidden');
      document.body.style.backgroundColor = "blue";
    } else if (tiebreak === 2) {
      document.getElementById("hufflepuff").removeAttribute('hidden');
     // document.getElementById("noanswer").setAttribute('hidden', 'hidden');
     // document.getElementById("gryffindor").setAttribute('hidden', 'hidden');
     // document.getElementById("ravenclaw").setAttribute('hidden', 'hidden');
     // document.getElementById("slytherin").setAttribute('hidden', 'hidden');
      document.body.style.backgroundColor = "yellow";
    } else if (tiebreak === 3) {
      document.getElementById("slytherin").removeAttribute('hidden');
     // document.getElementById("noanswer").setAttribute('hidden', 'hidden');
    //  document.getElementById("gryffindor").setAttribute('hidden', 'hidden');
    //  document.getElementById("ravenclaw").setAttribute('hidden', 'hidden');
    //  document.getElementById("hufflepuff").setAttribute('hidden', 'hidden');
      document.body.style.backgroundColor = "green";
    }
  }
  console.log(gryffindor);
  console.log(ravenclaw);
  console.log(hufflepuff);
  console.log(slytherin);

  event.preventDefault();
});}

//adding event listener for when user hits the reset button
if (form) {
form.addEventListener("reset", (event) => {
  //reset the values for the houses and reset the images
  gryffindor = 0;
  ravenclaw = 0;
  hufflepuff = 0;
  slytherin = 0;
  document.getElementById("gryffindor").setAttribute('hidden', 'hidden');
  document.getElementById("ravenclaw").setAttribute('hidden', 'hidden');
  document.getElementById("hufflepuff").setAttribute('hidden', 'hidden');
  document.getElementById("slytherin").setAttribute('hidden', 'hidden');
  document.getElementById("noanswer").setAttribute('hidden', 'hidden');
  document.body.style.backgroundColor = "white";
});}
