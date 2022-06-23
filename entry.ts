// @ts-ignore
import { Elm } from './src/Main.elm'

const app = Elm.Main.init({
    flags: {
        seed: Date.now(),
    },
})

app.ports.scrollToId.subscribe((id) => {
    const item = document.querySelector('#' + id)
    if (!item) return
    const scroller = document.querySelector('#program-explorer')
    if (!scroller) return
    scroller.scrollTop =
        scroller.scrollTop +
        (item.getBoundingClientRect().y -
            scroller.getBoundingClientRect().height / 2)
})

type Audio = {
    play(): void
    pause(): void
}

// Creates an audio object that is always playing but we control the volume to "pause" it.
// This maps better to chip8's boolean audio than creating fresh audio nodes every frame we
// want to play audio.
//
// - Can only play audio once user has interacted with the browser.
function makeAudio(): Audio {
    const context = new window.AudioContext()
    const osc = context.createOscillator()
    const vol = context.createGain()
    osc.type = 'square'
    osc.frequency.value = 80 // Hz
    osc.connect(vol)

    vol.gain.value = 0.1 // 0 (muted) to 1 (full volume)
    vol.connect(context.destination)

    let playing = false
    return {
        play() {
            if (!playing) {
                osc.start()
                playing = true
            }
            vol.gain.value = 0.1
        },
        pause() {
            if (playing) {
                vol.gain.value = 0.0
            }
        },
    }
}

// Wait until last second to create audio singleton.
let audio: Audio
app.ports.playAudio.subscribe((shouldPlay) => {
    if (!audio) {
        audio = makeAudio()
    }
    if (shouldPlay) {
        audio.play()
    } else {
        audio.pause()
    }
})
