;;; vmd-mode.el --- Fast Github-flavored Markdown preview using a vmd subprocess.

;; Copyright (C) 2016 Blake Miller

;; Author: Blake Miller <blak3mill3r@gmail.com>
;; Version: 0.2.0
;; Package-Version: 20160531.1719
;; Keywords: markdown, preview, live, vmd
;; URL: https://github.com/blak3mill3r/vmd-mode
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Realtime Markdown previews for Emacs, updates as the contents of the buffer
;; change.
;;
;; The rendered Markdown stays consistent with the markdown source in the
;; buffer, and performance is very good.

;;; Code:

(defvar-local vmd-process nil
  "Handle to the inferior vmd process")

(defvar-local vmd-preview-file nil
  "Temp file which is watched by the vmd process")

(defgroup vmd nil
  "Fast Github-flavored Markdown preview using a vmd subprocess."
  :prefix "vmd-"
  :group 'text)

(defcustom vmd-binary-path (executable-find "vmd")
  "Path to your vmd binary."
  :group 'vmd)

(defvar vmd-mode-github-emojis-list
  '(":" "-1:" "+1:" "100:" "1234:" "8ball:" "a:" "ab:" "abc:" "abcd:" "accept:"
    "aerial_tramway:" "airplane:" "alarm_clock:" "alien:" "ambulance:" "anchor:"
    "angel:" "anger:" "angry:" "anguished:" "ant:" "apple:" "aquarius:" "aries:"
    "arrow_backward:" "arrow_double_down:" "arrow_double_up:" "arrow_down:"
    "arrow_down_small:" "arrow_forward:" "arrow_heading_down:"
    "arrow_heading_up:" "arrow_left:" "arrow_lower_left:" "arrow_lower_right:"
    "arrow_right:" "arrow_right_hook:" "arrows_clockwise:"
    "arrows_counterclockwise:" "arrow_up:" "arrow_up_down:" "arrow_upper_left:"
    "arrow_upper_right:" "arrow_up_small:" "art:" "articulated_lorry:"
    "astonished:" "atm:" "b:" "baby:" "baby_bottle:" "baby_chick:"
    "baby_symbol:" "back:" "baggage_claim:" "balloon:" "ballot_box_with_check:"
    "bamboo:" "banana:" "bangbang:" "bank:" "barber:" "bar_chart:" "baseball:"
    "basketball:" "bath:" "bathtub:" "battery:" "bear:" "beer:" "beers:"
    "beetle:" "beginner:" "bell:" "bento:" "bicyclist:" "bike:" "bikini:"
    "bird:" "birthday:" "black_circle:" "black_joker:" "black_large_square:"
    "black_medium_small_square:" "black_medium_square:" "black_nib:"
    "black_small_square:" "black_square_button:" "blossom:" "blowfish:"
    "blue_book:" "blue_car:" "blue_heart:" "blush:" "boar:" "boat:" "bomb:"
    "book:" "bookmark:" "bookmark_tabs:" "books:" "boom:" "boot:" "bouquet:"
    "bow:" "bowling:" "bowtie:" "boy:" "bread:" "bride_with_veil:"
    "bridge_at_night:" "briefcase:" "broken_heart:" "bug:" "bulb:"
    "bullettrain_front:" "bullettrain_side:" "bus:" "busstop:"
    "bust_in_silhouette:" "busts_in_silhouette:" "cactus:" "cake:" "calendar:"
    "calling:" "camel:" "camera:" "cancer:" "candy:" "capital_abcd:"
    "capricorn:" "car:" "card_index:" "carousel_horse:" "cat:" "cat2:" "cd:"
    "chart:" "chart_with_downwards_trend:" "chart_with_upwards_trend:"
    "checkered_flag:" "cherries:" "cherry_blossom:" "chestnut:" "chicken:"
    "children_crossing:" "chocolate_bar:" "christmas_tree:" "church:" "cinema:"
    "circus_tent:" "city_sunrise:" "city_sunset:" "cl:" "clap:" "clapper:"
    "clipboard:" "clock1:" "clock10:" "clock1030:" "clock11:" "clock1130:"
    "clock12:" "clock1230:" "clock130:" "clock2:" "clock230:" "clock3:"
    "clock330:" "clock4:" "clock430:" "clock5:" "clock530:" "clock6:"
    "clock630:" "clock7:" "clock730:" "clock8:" "clock830:" "clock9:"
    "clock930:" "closed_book:" "closed_lock_with_key:" "closed_umbrella:"
    "cloud:" "clubs:" "cn:" "cocktail:" "coffee:" "cold_sweat:" "collision:"
    "computer:" "confetti_ball:" "confounded:" "confused:" "congratulations:"
    "construction:" "construction_worker:" "convenience_store:" "cookie:"
    "cool:" "cop:" "copyright:" "corn:" "couple:" "couplekiss:"
    "couple_with_heart:" "cow:" "cow2:" "credit_card:" "crescent_moon:"
    "crocodile:" "crossed_flags:" "crown:" "cry:" "crying_cat_face:"
    "crystal_ball:" "cupid:" "curly_loop:" "currency_exchange:" "curry:"
    "custard:" "customs:" "cyclone:" "dancer:" "dancers:" "dango:" "dart:"
    "dash:" "date:" "de:" "deciduous_tree:" "department_store:" "diamonds:"
    "diamond_shape_with_a_dot_inside:" "disappointed:" "disappointed_relieved:"
    "dizzy:" "dizzy_face:" "dog:" "dog2:" "dollar:" "dolls:" "dolphin:"
    "do_not_litter:" "door:" "doughnut:" "dragon:" "dragon_face:" "dress:"
    "dromedary_camel:" "droplet:" "dvd:" "ear:" "ear_of_rice:" "earth_africa:"
    "earth_americas:" "earth_asia:" "egg:" "eggplant:" "eight:"
    "eight_pointed_black_star:" "eight_spoked_asterisk:" "electric_plug:"
    "elephant:" "e-mail:" "email:" "end:" "envelope:" "es:" "euro:"
    "european_castle:" "european_post_office:" "evergreen_tree:" "exclamation:"
    "expressionless:" "eyeglasses:" "eyes:" "facepunch:" "factory:"
    "fallen_leaf:" "family:" "fast_forward:" "fax:" "fearful:" "feelsgood:"
    "feet:" "ferris_wheel:" "file_folder:" "finnadie:" "fire:" "fire_engine:"
    "fireworks:" "first_quarter_moon:" "first_quarter_moon_with_face:" "fish:"
    "fish_cake:" "fishing_pole_and_fish:" "fist:" "five:" "flags:" "flashlight:"
    "floppy_disk:" "flower_playing_cards:" "flushed:" "foggy:" "football:"
    "fork_and_knife:" "fountain:" "four:" "four_leaf_clover:" "fr:" "free:"
    "fried_shrimp:" "fries:" "frog:" "frowning:" "fu:" "fuelpump:" "full_moon:"
    "full_moon_with_face:" "game_die:" "gb:" "gem:" "gemini:" "ghost:" "gift:"
    "gift_heart:" "girl:" "globe_with_meridians:" "goat:" "goberserk:"
    "godmode:" "golf:" "grapes:" "green_apple:" "green_book:" "green_heart:"
    "grey_exclamation:" "grey_question:" "grimacing:" "grin:" "grinning:"
    "guardsman:" "guitar:" "gun:" "haircut:" "hamburger:" "hammer:" "hamster:"
    "hand:" "handbag:" "hankey:" "hash:" "hatched_chick:" "hatching_chick:"
    "headphones:" "hear_no_evil:" "heart:" "heartbeat:" "heart_decoration:"
    "heart_eyes:" "heart_eyes_cat:" "heartpulse:" "hearts:" "heavy_check_mark:"
    "heavy_division_sign:" "heavy_dollar_sign:" "heavy_exclamation_mark:"
    "heavy_minus_sign:" "heavy_multiplication_x:" "heavy_plus_sign:"
    "helicopter:" "herb:" "hibiscus:" "high_brightness:" "high_heel:" "hocho:"
    "honeybee:" "honey_pot:" "horse:" "horse_racing:" "hospital:" "hotel:"
    "hotsprings:" "hourglass:" "hourglass_flowing_sand:" "house:"
    "house_with_garden:" "hurtrealbad:" "hushed:" "ice_cream:" "icecream:" "id:"
    "ideograph_advantage:" "imp:" "inbox_tray:" "incoming_envelope:"
    "information_desk_person:" "information_source:" "innocent:" "interrobang:"
    "iphone:" "it:" "izakaya_lantern:" "jack_o_lantern:" "japan:"
    "japanese_castle:" "japanese_goblin:" "japanese_ogre:" "jeans:" "joy:"
    "joy_cat:" "jp:" "key:" "keycap_ten:" "kimono:" "kiss:" "kissing:"
    "kissing_cat:" "kissing_closed_eyes:" "kissing_heart:"
    "kissing_smiling_eyes:" "koala:" "koko:" "kr:" "large_blue_circle:"
    "large_blue_diamond:" "large_orange_diamond:" "last_quarter_moon:"
    "last_quarter_moon_with_face:" "laughing:" "leaves:" "ledger:"
    "left_luggage:" "left_right_arrow:" "leftwards_arrow_with_hook:" "lemon:"
    "leo:" "leopard:" "libra:" "light_rail:" "link:" "lips:" "lipstick:" "lock:"
    "lock_with_ink_pen:" "lollipop:" "loop:" "loudspeaker:" "love_hotel:"
    "love_letter:" "low_brightness:" "m:" "mag:" "mag_right:" "mahjong:"
    "mailbox:" "mailbox_closed:" "mailbox_with_mail:" "mailbox_with_no_mail:"
    "man:" "mans_shoe:" "man_with_gua_pi_mao:" "man_with_turban:" "maple_leaf:"
    "mask:" "massage:" "meat_on_bone:" "mega:" "melon:" "memo:" "mens:" "metal:"
    "metro:" "microphone:" "microscope:" "milky_way:" "minibus:" "minidisc:"
    "mobile_phone_off:" "moneybag:" "money_with_wings:" "monkey:" "monkey_face:"
    "monorail:" "mortar_board:" "mountain_bicyclist:" "mountain_cableway:"
    "mountain_railway:" "mount_fuji:" "mouse:" "mouse2:" "movie_camera:"
    "moyai:" "muscle:" "mushroom:" "musical_keyboard:" "musical_note:"
    "musical_score:" "mute:" "nail_care:" "name_badge:" "neckbeard:" "necktie:"
    "negative_squared_cross_mark:" "neutral_face:" "new:" "new_moon:"
    "new_moon_with_face:" "newspaper:" "ng:" "nine:" "no_bell:" "no_bicycles:"
    "no_entry:" "no_entry_sign:" "no_good:" "no_mobile_phones:" "no_mouth:"
    "non-potable_water:" "no_pedestrians:" "nose:" "no_smoking:" "notebook:"
    "notebook_with_decorative_cover:" "notes:" "nut_and_bolt:" "o:" "o2:"
    "ocean:" "octocat:" "octopus:" "oden:" "office:" "ok:" "ok_hand:"
    "ok_woman:" "older_man:" "older_woman:" "on:" "oncoming_automobile:"
    "oncoming_bus:" "oncoming_police_car:" "oncoming_taxi:" "one:"
    "open_file_folder:" "open_hands:" "open_mouth:" "ophiuchus:" "orange_book:"
    "outbox_tray:" "ox:" "package:" "page_facing_up:" "pager:" "page_with_curl:"
    "palm_tree:" "panda_face:" "paperclip:" "parking:" "part_alternation_mark:"
    "partly_sunny:" "passport_control:" "paw_prints:" "peach:" "pear:" "pencil:"
    "pencil2:" "penguin:" "pensive:" "performing_arts:" "persevere:"
    "person_frowning:" "person_with_blond_hair:" "person_with_pouting_face:"
    "phone:" "pig:" "pig2:" "pig_nose:" "pill:" "pineapple:" "pisces:" "pizza:"
    "point_down:" "point_left:" "point_right:" "point_up:" "point_up_2:"
    "police_car:" "poodle:" "poop:" "postal_horn:" "postbox:" "post_office:"
    "potable_water:" "pouch:" "poultry_leg:" "pound:" "pouting_cat:" "pray:"
    "princess:" "punch:" "purple_heart:" "purse:" "pushpin:"
    "put_litter_in_its_place:" "question:" "rabbit:" "rabbit2:" "racehorse:"
    "radio:" "radio_button:" "rage:" "rage1:" "rage2:" "rage3:" "rage4:"
    "railway_car:" "rainbow:" "raised_hand:" "raised_hands:" "raising_hand:"
    "ram:" "ramen:" "rat:" "recycle:" "red_car:" "red_circle:" "registered:"
    "relaxed:" "relieved:" "repeat:" "repeat_one:" "restroom:"
    "revolving_hearts:" "rewind:" "ribbon:" "rice:" "rice_ball:" "rice_cracker:"
    "rice_scene:" "ring:" "rocket:" "roller_coaster:" "rooster:" "rose:"
    "rotating_light:" "round_pushpin:" "rowboat:" "ru:" "rugby_football:"
    "runner:" "running:" "running_shirt_with_sash:" "sa:" "sagittarius:"
    "sailboat:" "sake:" "sandal:" "santa:" "satellite:" "satisfied:"
    "saxophone:" "school:" "school_satchel:" "scissors:" "scorpius:" "scream:"
    "scream_cat:" "scroll:" "seat:" "secret:" "seedling:" "see_no_evil:"
    "seven:" "shaved_ice:" "sheep:" "shell:" "ship:" "shipit:" "shirt:" "shit:"
    "shoe:" "shower:" "signal_strength:" "six:" "six_pointed_star:" "ski:"
    "skull:" "sleeping:" "sleepy:" "slot_machine:" "small_blue_diamond:"
    "small_orange_diamond:" "small_red_triangle:" "small_red_triangle_down:"
    "smile:" "smile_cat:" "smiley:" "smiley_cat:" "smiling_imp:" "smirk:"
    "smirk_cat:" "smoking:" "snail:" "snake:" "snowboarder:" "snowflake:"
    "snowman:" "sob:" "soccer:" "soon:" "sos:" "sound:" "space_invader:"
    "spades:" "spaghetti:" "sparkle:" "sparkler:" "sparkles:" "sparkling_heart:"
    "speaker:" "speak_no_evil:" "speech_balloon:" "speedboat:" "squirrel:"
    "star:" "star2:" "stars:" "station:" "statue_of_liberty:"
    "steam_locomotive:" "stew:" "straight_ruler:" "strawberry:"
    "stuck_out_tongue:" "stuck_out_tongue_closed_eyes:"
    "stuck_out_tongue_winking_eye:" "sunflower:" "sunglasses:" "sunny:"
    "sunrise:" "sunrise_over_mountains:" "sun_with_face:" "surfer:" "sushi:"
    "suspect:" "suspension_railway:" "sweat:" "sweat_drops:" "sweat_smile:"
    "sweet_potato:" "swimmer:" "symbols:" "syringe:" "tada:" "tanabata_tree:"
    "tangerine:" "taurus:" "taxi:" "tea:" "telephone:" "telephone_receiver:"
    "telescope:" "tennis:" "tent:" "thought_balloon:" "three:" "thumbsdown:"
    "thumbsup:" "ticket:" "tiger:" "tiger2:" "tired_face:" "tm:" "toilet:"
    "tokyo_tower:" "tomato:" "tongue:" "top:" "tophat:" "tractor:"
    "traffic_light:" "train:" "train2:" "tram:" "triangular_flag_on_post:"
    "triangular_ruler:" "trident:" "triumph:" "trolleybus:" "trollface:"
    "trophy:" "tropical_drink:" "tropical_fish:" "truck:" "trumpet:" "tshirt:"
    "tulip:" "turtle:" "tv:" "twisted_rightwards_arrows:" "two:" "two_hearts:"
    "two_men_holding_hands:" "two_women_holding_hands:" "u5272:" "u5408:"
    "u55b6:" "u6307:" "u6708:" "u6709:" "u6e80:" "u7121:" "u7533:" "u7981:"
    "u7a7a:" "uk:" "umbrella:" "unamused:" "underage:" "unlock:" "up:" "us:"
    "v:" "vertical_traffic_light:" "vhs:" "vibration_mode:" "video_camera:"
    "video_game:" "violin:" "virgo:" "volcano:" "vs:" "waning_crescent_moon:"
    "waning_gibbous_moon:" "warning:" "watch:" "water_buffalo:" "watermelon:"
    "wave:" "wavy_dash:" "waxing_crescent_moon:" "waxing_gibbous_moon:" "wc:"
    "weary:" "wedding:" "whale:" "whale2:" "wheelchair:" "white_check_mark:"
    "white_circle:" "white_flower:" "white_large_square:"
    "white_medium_small_square:" "white_medium_square:" "white_small_square:"
    "white_square_button:" "wind_chime:" "wine_glass:" "wink:" "wolf:" "woman:"
    "womans_clothes:" "womans_hat:" "womens:" "worried:" "wrench:" "x:"
    "yellow_heart:" "yen:" "yum:" "zap:" "zero:" "zzz:"))

(defun vmd-mode-start-vmd-process ()
  "Start an asynchronous `vmd' process to generate the `vmd-preview-file' file."
  (setq vmd-preview-file (make-temp-file "vmd-preview"))
  (setq vmd-process (start-process "vmd" "vmd" vmd-binary-path vmd-preview-file)))

(defun vmd-mode-refresh (&rest args)
  "Update the `vmd-preview-file'.
The optional ARGS argument is needed as this function is added to the
`after-change-functions' hook."
  (write-region (point-min) (point-max) vmd-preview-file))

;;;###autoload
(define-minor-mode vmd-mode
  "Live Markdown preview with `vmd'."
  :lighter " vmd"
  (if vmd-mode
      (if vmd-binary-path
          (progn
            (add-hook 'after-change-functions #'vmd-mode-refresh nil t)
            (vmd-mode-start-vmd-process)
            (vmd-mode-refresh))
        (user-error "You need to have `vmd' installed in your environment PATH."))
    (progn
      (delete-process vmd-process)
      (remove-hook 'after-change-functions #'vmd-mode-refresh t))))


(provide 'vmd-mode)

;;; vmd-mode.el ends here
