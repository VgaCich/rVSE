unit GameData;

interface

uses
  GameObjects;
  
const
  SPlague = 'Plague';
  SBachelor = 'Bachelor';
  SHaruspex = 'Haruspex';
  SChangeling = 'Changeling';
  SMariaKaina = 'MariaKaina';
  SVladTheYounger = 'VladTheYounger';
  SAndreyStamatin = 'AndreyStamatin';
  SCapella = 'Capella';
  SNotkin = 'Notkin';
  SSticky = 'Sticky';
  SAlexanderSaburov = 'AlexanderSaburov';
  SStanislavRubin = 'StanislavRubin';
  SAspity = 'Aspity';
  CharBachelor: TCharProfile = (
    RuName: 'Бакалавр';
    IsDoctor: true;
    Master: '';
    Resource: rtCoin;
  );
  CharHaruspex: TCharProfile = (
    RuName: 'Гаруспик';
    IsDoctor: true;
    Master: '';
    Resource: rtSecret;
  );
  CharChangeling: TCharProfile = (
    RuName: 'Самозванка';
    IsDoctor: true;
    Master: '';
    Resource: rtKey;
  );
  CharMariaKaina: TCharProfile = (
    RuName: 'Мария Каина';
    IsDoctor: false;
    Master: SBachelor;
    Resource: rtCoin;
  );
  CharVladTheYounger: TCharProfile = (
    RuName: 'Младший Влад';
    IsDoctor: false;
    Master: SBachelor;
    Resource: rtSecret;
  );
  CharAndreyStamatin: TCharProfile = (
    RuName: 'Андрей Стаматин';
    IsDoctor: false;
    Master: SBachelor;
    Resource: rtKey;
  );
  CharCapella: TCharProfile = (
    RuName: 'Капелла';
    IsDoctor: false;
    Master: SHaruspex;
    Resource: rtCoin;
  );
  CharNotkin: TCharProfile = (
    RuName: 'Ноткин';
    IsDoctor: false;
    Master: SHaruspex;
    Resource: rtSecret;
  );
  CharSticky: TCharProfile = (
    RuName: 'Спичка';
    IsDoctor: false;
    Master: SHaruspex;
    Resource: rtKey;
  );
  CharAlexanderSaburov: TCharProfile = (
    RuName: 'Александр Сабуров';
    IsDoctor: false;
    Master: SChangeling;
    Resource: rtCoin;
  );
  CharStanislavRubin: TCharProfile = (
    RuName: 'Станислав Рубин';
    IsDoctor: false;
    Master: SChangeling;
    Resource: rtSecret;
  );
  CharAspity: TCharProfile = (
    RuName: 'Оспина';
    IsDoctor: false;
    Master: SChangeling;
    Resource: rtKey;
  );
  Characters: array[0..11] of record
    Name: string;
    Profile: ^TCharProfile;
  end = (
    (Name: SBachelor; Profile: @CharBachelor),
    (Name: SHaruspex; Profile: @CharHaruspex),
    (Name: SChangeling; Profile: @CharChangeling),
    (Name: SMariaKaina; Profile: @CharMariaKaina),
    (Name: SVladTheYounger; Profile: @CharVladTheYounger),
    (Name: SAndreyStamatin; Profile: @CharAndreyStamatin),
    (Name: SCapella; Profile: @CharCapella),
    (Name: SNotkin; Profile: @CharNotkin),
    (Name: SSticky; Profile: @CharSticky),
    (Name: SAlexanderSaburov; Profile: @CharAlexanderSaburov),
    (Name: SStanislavRubin; Profile: @CharStanislavRubin),
    (Name: SAspity; Profile: @CharAspity));
  {Chips: array[0..6] of string = (
    'Chips\Bachelor.png',
    'Chips\Changeling.png',
    'Chips\Haruspex.png',
    'Chips\Plague.png',
    'Chips\Coin.png',
    'Chips\Key.png',
    'Chips\Secret.png');}
    
implementation

end.