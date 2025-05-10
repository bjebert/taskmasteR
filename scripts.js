$(function(){ 
    $(document).keyup(function(e) {
        if (e.code == 'Enter') {
            $('#createTask').click()
        }
    });
});


$(document).on('click', 'input', function () {
    Shiny.onInputChange('user_clicked',this.id);
});


/*$(document).ready(function(){
    resizePanel();
    $(window).resize(function(){
        resizePanel();
    });
});


function resizePanel() {
    let h = $(window).height() * 0.9;  // or whatever percentage you want
    document.getElementById("taskUiPanel").style.maxHeight = h + "px";
}*/



