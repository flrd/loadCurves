$(function(){
    $('input[type="text"]').change(function(){
        this.value = $.trim(this.value);
    });
});