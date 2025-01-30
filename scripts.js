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



