function initGestures(win) {
  var tap = new Event('gesture.tap');
  var longtap = new Event('gesture.longtap');

  var gestureInProgress;
  var pressTimout;
  var longPressDone;

  function clearGestureSession() {
    gestureInProgress = false;
    longPressDone = false;
    clearTimeout(pressTimout);
    pressTimout = null;
  }

  clearGestureSession();

  function onTouchStart(event) {
    if (!gestureInProgress) {
      gestureInProgress = true;
      if (event.touches.length == 1) {
        pressTimout = setTimeout(function() {
          longPressDone = true;
          event.target.dispatchEvent(longtap);
        }, 500);
      }
    }
  }

  function onTouchEnd(event) {
    if (event.touches.length == 0 && !longPressDone) {
      event.target.dispatchEvent(tap);
    }
    clearGestureSession();
  }

  function onTouchCancell(event) {
    clearGestureSession();
  }

  win.addEventListener("touchstart", onTouchStart);
  win.addEventListener("touchend", onTouchEnd);
  win.addEventListener("touchcancell", onTouchCancell);
}

module.exports = initGestures;
