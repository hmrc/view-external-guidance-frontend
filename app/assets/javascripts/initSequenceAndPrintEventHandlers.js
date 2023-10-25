var ready = (callback) => {
    if (document.readyState != "loading") callback();
    else document.addEventListener("DOMContentLoaded", callback);
  }

  ready(() => {

    function initCheckboxEventHandlers(checkboxes){

      for(var i = 0; i < checkboxes.length; i++){

        if(isExclusiveCheckbox(checkboxes[i])){

          checkboxes[i].addEventListener("change", handleExclusiveCheckboxSelection);

        } else {

          checkboxes[i].addEventListener("change", handleNonExclusiveCheckboxSelection);

        }
      }

    }

    function handleNonExclusiveCheckboxSelection(event){

      if(event.target.checked){

        var checkboxes = document.getElementsByClassName("govuk-checkboxes__input");

        for(var i = 0; i < checkboxes.length; i++){
          if(isExclusiveCheckbox(checkboxes[i])){
            checkboxes[i].checked = false;
          }
        }

      }
    }

    function handleExclusiveCheckboxSelection(event){

      if(event.target.checked){

        var checkboxes = document.getElementsByClassName("govuk-checkboxes__input");

        for(var i = 0; i < checkboxes.length; i++){
          if(!isExclusiveCheckbox(checkboxes[i])){
            checkboxes[i].checked = false;
          }
        }

      }
    }

    function isExclusiveCheckbox(checkbox){
      var exclusive = false;
      if(checkbox.hasAttribute("data-exclusive") && checkbox.getAttribute("data-exclusive") == "true")
        exclusive = true;

      return exclusive;
    }

    function initPrintDialogHandler(elements){
      var counter = 0;
      while(counter < elements.length) {
        if (elements[counter].id.substring(0,  "print-dialog-".length) == "print-dialog-") {
          elements[counter].addEventListener("click", function() {
            event.preventDefault()
            window.print()
          });
        }
        counter++;
      }
    }

    var checkboxes = document.getElementsByClassName("govuk-checkboxes__input");

    if(checkboxes.length > 0){

      var foundExclusiveCheckbox = false;
      var counter = 0;

      while(counter < checkboxes.length && !foundExclusiveCheckbox){
        if(isExclusiveCheckbox(checkboxes[counter])){
          foundExclusiveCheckbox = true;
        }
        counter++;
      }

      if(foundExclusiveCheckbox)
        initCheckboxEventHandlers(checkboxes);

    }

    initPrintDialogHandler(document.getElementsByClassName("govuk-link"));
    initPrintDialogHandler(document.getElementsByClassName("govuk-button"));

  });

