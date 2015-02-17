#! c:\perl\bin\

###########################################################################
#                                                                         #
#                         H A P L O P A I N T E R                         #
#                                                                         #
#      +==========================================================+       #
#      |                                                          |       #
#      |  Copyright (c) 2007 Holger Thiele. All rights reserved.  |       #
#      |              This program is free software.              |       #
#      |         You can redistribute it and/or modify it         |       #
#      |       under the terms of GNU General Public License      |       #
#      |        as published by the Free Software Foundation.     |       #
#      |                                                          |       #
#      +==========================================================+       #
#                                                                         #
#                                                                         #
###########################################################################

use warnings;
use strict;
use File::Basename;
use File::Spec::Functions;
use vars qw / $mw $canvas $menubar $opt $self $grid %pedigree %haplo %map $batch/ ;
use subs qw / _MainMenu _ContextMenu /;
use Tk;
use Tk::DialogBox;
use Tk::BrowseEntry;
use Tk::NoteBook;
use Tk ':variables';
use Sort::Naturally;
use Storable qw /freeze thaw retrieve store dclone/;
use Data::Dumper;
use DBI;
$Data::Dumper::Sortkeys =1;


###########################################################################

### Hash for global variables - not family specific
my $param = {
	VERSION			=> '029.5',
	LAST_CHANGE		=> '31.5.07',
	PAPER			=> 'A4',
	ORIENTATION 	=> 'Landscape',
	PAPER_SIZE		=> {
		A0 		=> { X => 840, Y => 1189 },
		A1 		=> { X => 594, Y => 840  },
		A2 		=> { X => 420, Y => 594  },
		A3 		=> { X => 297, Y => 420  },
		A4 		=> { X => 210, Y => 297  },
		A5 		=> { X => 148, Y => 210  },
		B4 		=> { X => 257, Y => 364  },
		B5 		=> { X => 182, Y => 257  },
		Ledger 	=> { X => 432, Y => 279  },
		Letter 	=> { X => 216, Y => 279  },
		Legal 	=> { X => 216, Y => 356  },
		'11x17' => { X => 279, Y => 432  }
	},
	PRINT_SUPPORT	=> { MSWin32 => 'gswin32c', linux => 'gs' },
	BORDER_UP    	=> 	25,
	BORDER_DOWN    	=> 	25,
	BORDER_LEFT    	=> 	25,
	BORDER_RIGHT    => 	25,
	GRAPHIC_FORMAT  =>  'postscript',
	GRAPHIC_FORMATS_OK => { qw / jpeg jpg png16m png pngalpha png postscript ps pdfwrite pdf Print ps/ },
	RESOLUTION      =>  288,
	TEXT_ALPHA_BITS	=> 4,
	GRAPHICS_ALPHA_BITS => 4,
	DEFAULT			=> {},
	LOOP_BREAK_STATUS	=> 0,
	SHOW_HEAD => 1,
	SHOW_DATE => 0,
	SHOW_PEDFILE => 0,
	SHOW_HAPLOFILE => 0,
	DB_SID => '',
	DB_HOST => '',
	DB_UNAME => '',
	DB_RELATION => '',
	STATUS => 0
};



############################################################################
#  family specific variable $self->{$struk} as nested array of arrays of ....
#  holds pedigree structure Hierachy: Generation->Sibgroups->Person/Couples
#  Sibgroups are considered as 'extended' in sense of sibs + spouses 
#  
#  
#
#
#  $struk =
#  [
#     [ Generation 1 ],                generation
#     [ Generation 2 ],                
#     [                                
#        [ Sibs 1 ],                    extended sibgroup
#        [ Sibs 2 ],                   
#        [                             
#           Pid 1,                      sib without spouses
#           [ Partner 1 ],              sib with spouses
#           [                       
#              [ p1, p2, p3 ],          drawing order of multiple mates in one row
#              [ p1, p3 ] , [ p2, p3 ]  reflect sib groups drived from [ p1, p2, p3 ]
#              [ p1, p2 ] , [ p2, p3 ]  reflect *real* drawing order of sib groups
#           ]
#        ]
#     ]
#  ]
#
#
###########################################################################


# MakeSelf is called for every family in drawing focus. 
# It holds object orientated all subvariables in one hash. 
# This structure is saved later by Storable.pm
#=============
sub MakeSelf {
#=============	
	my $family = shift || '';
	$self = {
		AFF_COLOR       => { 0 => 'grey80', 1 => 'white', 2 => 'black' },
		SHOW_QUEST		=> 1,
		LINE_COLOR		=> 'black',
		BACKGROUND		=> '#ffffff',
		COUNT			=>  1,
		CROSS_FAKTOR1   =>  1,
		CONSANG_DIST	=>	4,
		GITTER_X		=>  25,
		GITTER_Y		=>  25,
		SYMBOL_SIZE		=>  26,
		FONT1			=> { 	FAMILY 	=> 'Lucida',
								SIZE	=> 16,
								WEIGHT 	=> 'bold',
								SLANT	=> 'roman',
								COLOR	=> 'black'
							},
		FONT_HAPLO		=> { 	FAMILY 	=> 'Lucida',
								SIZE	=> 14,
								WEIGHT 	=> 'bold',
								SLANT	=> 'roman',
								COLOR	=> 'black'
							},
		FONT_HEAD		=> { 	FAMILY 	=> 'Lucida',
								SIZE	=> 30,
								WEIGHT 	=> 'bold',
								SLANT	=> 'roman',
								COLOR	=> 'black'
							},
		CASE_INFO_SHOW => { 
									1 => 1,
									2 => 1,
									3 => 1,
									4 => 1
								},
		ZOOM			=>  1,
		LINE_WIDTH		=>  1.5,
		X_SPACE 		=>  3,
		Y_SPACE 		=>  6,
		Y_SPACE_EXTRA	=>  0.5,
		Y_SPACE_DEFAULT	=>	6,
		STRUK			=>  [  ],
		MATRIX			=>  {},
		LINES			=>  {},
		HAPLO			=> undef,
		FAMILY			=> $family,
		FILENAME		=> "$family.dump",
		CROSS_LOOP		=> 6,
		LEGEND_SHIFT_LEFT	=> 200,
		LEGEND_SHIFT_RIGHT	=> 50,
		MARKER_POS_SHIFT	=> 155,
		ALLELES_SHIFT	=> 15,
		HAPLO_UNKNOWN	=> '0',
		HAPLO_UNKNOWN_COLOR	=> 'black',
		HAPLO_TEXT_LW	=> 0,
		SHOW_HAPLO_TEXT	=> 1,
		SHOW_HAPLO_BAR	=> 1,
		SHOW_HAPLO_NI_0	=> 1,
		SHOW_HAPLO_NI_1	=> 1,
		SHOW_HAPLO_NI_2	=> 1,
		SHOW_HAPLO_NI_3	=> 0,
		HAPLO_SEP_BL	=> 0,
		FILL_HAPLO		=> 1,
		HAPLO_WIDTH		=> 12,
		HAPLO_WIDTH_NI	=> 4,
		HAPLO_SPACE		=> 9,
		HAPLO_LW		=> 1,
		SHOW_MARKER		=> 1,
		SHOW_POSITION	=> 1,
		SHOW_LEGEND_LEFT	=> 1,
		SHOW_LEGEND_RIGHT	=> 0,
		SHOW_COLORED_TEXT => 0,
		ALIGN_LEGEND	=> 1,
		SHOW_DATE		=> $param->{SHOW_DATE},
		SHOW_HEAD		=> $param->{SHOW_HEAD},
		SHOW_HAPLOFILE	=> $param->{SHOW_HAPLOFILE},
		SHOW_PEDFILE	=> $param->{SHOW_PEDFILE},
		SHOW_HAPLO_BBOX	=> 1,
		BBOX_WIDTH		=> 35,
		ALIVE_SPACE		=> 5,
		PEDIGREE_PATH	=> $param->{PEDIGREE_PATH},
		HAPLO_PATH		=> $param->{HAPLO_PATH},
	};
	
	### transfer of haplotype and map information from multiple pedigrees 
	if ($family && defined $haplo{$family}) {
		$self->{HAPLO} = $haplo{$family};
		$self->{HAPLO}{MAP} = \%map if	%map;
	}	
}


###########################################################################

### Starting the program
Main();


###########################################################################
#
#                              Methods
#
###########################################################################



# Building Main Window, Canvas and some bindings
#=========
sub Main {
#=========
	
	MakeSelf();
	
	### prepare batch mode
	if ($ARGV[0]) { $batch=1; PrepareBatch() }	
		
	Default('update');
	
	my $f = $self->{FONT_HAPLO};
	my $z = $self->{ZOOM};

	$mw = MainWindow->new(-title => "HaploPainter V.$param->{VERSION}");
	$mw->withdraw;
	my $scr_x  = $mw->screenwidth;
	my $scr_y  = $mw->screenheight;
	my $mw_szx = 0.8;
	my $mw_szy = 0.6;

	$mw->geometry (
		int($scr_x*$mw_szx) . 'x' . int($scr_y * $mw_szy) .  '+' .
		int($scr_x*(1-$mw_szx)/2) . '+' . int($scr_y * (1-$mw_szy)/3)
	);
		
	### Attaching the menu from Main Window
	$mw->configure(-menu => $menubar = $mw->Menu(-menuitems => _MainMenu));

	### proper view of font size
	$mw->scaling(1);

	$canvas = $mw->Scrolled(
		'Canvas',-width => 10000, -height => 10000, -bg => $self->{BACKGROUND},
		-scrollbars => 'osoe', 
		-scrollregion => [ 0,0,2000,2000],-cursor => 'left_ptr',
	)->pack(-padx => 3, -pady => 3, -expand => 1, -fill => 'both');


	my $menu = $canvas->Menu(-tearoff => 0, -menuitems => _ContextMenu);

	$canvas->CanvasBind('<1>' => [ \&ActivateSymbol, Ev('x'), Ev('y') ]);
	$canvas->CanvasBind('<3>' => [ \&ShowContextMenue, $menu, Ev('x'), Ev('y') ]); 
	$canvas->CanvasBind('<Configure>' => sub { AdjustView() });
	$canvas->CanvasBind('<MouseWheel>' =>
		[ sub { $_[0]->yview('scroll',-($_[1]/120),'units') }, Tk::Ev('D') ]
	);
		
	$canvas->bind('SYMBOL','<B1-Motion>', [ \&MouseB1Move, Ev('x'),Ev('y') ]);
	$canvas->bind('SYMBOL','<ButtonRelease-1>', [ \&MouseB1Release, Ev('x'),Ev('y') ] );
	$canvas->bind('HEAD', '<ButtonRelease-1>', [ \&HeadB1Release, Ev('x'),Ev('y') ] );
	$canvas->bind('HEAD', '<B1-Motion>' => [ \&MoveHead, Ev('x'), Ev('y') ] );
	
	$canvas->bind('ALLEL', '<Leave>', sub {
		$canvas->itemconfigure($param->{ACTIVE_ITEM}, -fill => $param->{ACTIVE_COLOUR});
		delete $param->{ACTIVE_ITEM}
	});
	$canvas->bind('ALLEL', '<Double-1>', \&KlickAllel  );
	$canvas->bind('SYMBOL','<Double-1>', \&KlickSymbol );
	$canvas->bind('ALLEL', '<Enter>', \&EnterAllel );

	### some ugly cursor shapes ( there is no minus symbol, thats live ! )
	$mw->bind('<KeyPress-Shift_L>'	=> sub { $canvas->configure(-cursor => 'plus'); $param->{STATUS} = 1  });
	$mw->bind('<KeyRelease-Shift_L>'=> sub { $canvas->configure(-cursor => 'left_ptr'); $param->{STATUS} = 0 });
	$mw->bind('<KeyPress-Control_L>'=> sub { $canvas->configure(-cursor => 'target'); $param->{STATUS} = 1  });
	$mw->bind('<KeyRelease-Control_L>'=> sub { $canvas->configure(-cursor => 'left_ptr'); $param->{STATUS} = 0 });
	$mw->bind('<Shift-1>'			=> [ \&Zoom,  1, 1, Ev('x'),Ev('y') ]  );
	$mw->bind('<Control-1>' 	=> [ \&Zoom, -1, 1, Ev('x'),Ev('y') ]  );		
	$mw->bind('<Control-p>' => sub { Print() } );	
	$mw->bind('<Control-Key-1>' => sub { ImportMapfile(1) } );
	$mw->bind('<Control-Key-2>' => sub { ImportMapfile(2) } );	
	$mw->bind('<Key-F1>' => sub { ImportPedfile('PRAEMAKEPED') });
	$mw->bind('<Key-F2>' => sub { ImportPedfile('PRAEMAKEPED_PLUS') });
	$mw->bind('<Key-F3>' => sub { ImportPedfile('POSTMAKEPED') });
	$mw->bind('<Key-F4>' => sub { ImportPedegreeDBI() });		
	$mw->bind('<Key-F5>' => sub { DrawPed() }); 
	$mw->bind('<Key-F6>' => sub { RedrawHaploShuffle() });
	$mw->bind('<Key-F7>' => sub { AdjustView() } );
	$mw->bind('<Key-F8>' => sub { AdjustView(-fit => 'center') });	
	$mw->bind('<Key-F9>'  => sub { ImportHaplofile('SIMWALK')	 });
	$mw->bind('<Key-F10>' => sub { ImportHaplofile('GENEHUNTER')	 });
	$mw->bind('<Key-F11>' => sub { ImportHaplofile('MERLIN')	 });
	$mw->bind('<Key-F12>' => sub { ImportHaplofile('ALLEGRO')	 });
	
	# MainWindow icon
 	$mw->idletasks; 
 	$mw->iconimage($mw->Photo(-format => 'gif', -data => GetIconData()));
 	
 	# batch modus ausf�hren
 	if ($batch) { $mw->afterIdle(\&BatchProcess) } else { 		
 		$mw->deiconify;
 		$mw->raise;
 	}
	MainLoop;	
}

# building the whole menu staff at once
#==============
sub _MainMenu {
#==============
	[
		map ['cascade', $_->[0], -menuitems => $_->[1], -tearoff => 0 ],
		[ '~File',
			[
				[ 'command', 'Open ...',	-command => \&RestoreSelf ],
				[ 'command', 'Save', 		-command => [\&SaveSelf, 0] ],
				[ 'command', 'Save as ..',	-command => [\&SaveSelf, 1] ],				
				,'-',
				[ 'command', 'Open Defaults ...',	-command => [\&Default, 'open' ] ],
				[ 'command', 'Save Defaults as ..',	-command => [\&Default, 'save' ] ],	
				,'-',
				[ 'cascade', 'Import Pedigrees ...', -tearoff => 0,	-menuitems =>
					[
						['command', 'Prae-Makeped',			-command => [ \&ImportPedfile, 'PRAEMAKEPED' ], -accelerator => 'F1' ],
						['command', 'Prae-Makeped-Plus',	-command => [ \&ImportPedfile, 'PRAEMAKEPED_PLUS' ], -accelerator => 'F2' ],
						['command', 'Post-Makeped',			-command => [ \&ImportPedfile, 'POSTMAKEPED' ], -accelerator => 'F3' ],
						['command', 'Database',			-command => \&ImportPedegreeDBI, -accelerator => 'F4' ],
					]
				],
				[ 'cascade', 'Import Haplotypes ...', -tearoff => 0,	-menuitems =>
					[
						['command', 'Simwalk',		-command => [ \&ImportHaplofile, 'SIMWALK'		], -accelerator => 'F9' ],
						['command', 'GeneHunter',	-command => [ \&ImportHaplofile, 'GENEHUNTER'	], -accelerator => 'F10' ],
						['command', 'Merlin',		-command => [ \&ImportHaplofile, 'MERLIN' 		], -accelerator => 'F11' ],
						['command', 'Allegro',		-command => [ \&ImportHaplofile, 'ALLEGRO' 		], -accelerator => 'F12' ],
					]
				],
				[ 'cascade', 'Import Map File ...', -tearoff => 0,	-menuitems =>
					[
						['command', '1 (CHR-POS-MARKER)',	-command => [ \&ImportMapfile, 1	], -accelerator => 'Contr+1'  ],
						['command', '2 (CHR-MARKER-POS)',	-command => [ \&ImportMapfile, 2	], -accelerator => 'Contr+2'  ],
					]
				],
				'-',
				[ 'cascade', 'Export ...', -tearoff => 0, -menuitems =>
					[				
						[ 'cascade', 'Export current ...', -tearoff => 0, -menuitems =>
							[
								[ 'command', 'POSTSCRIPT', 	-command => [ \&Export, 'postscript']  ],
								[ 'command', 'PNG', 		-command => [ \&Export, 'png16m']  ],
								[ 'command', 'PNG ALPHA', 	-command => [ \&Export, 'pngalpha']  ],
								[ 'command', 'JPEG', 		-command => [ \&Export, 'jpeg']  ],
								[ 'command', 'PDF', 		-command => [ \&Export, 'pdfwrite']  ]							
							],
						],	
						[ 'cascade', 'Export all ...', -tearoff => 0, -menuitems =>
							[
								[ 'command', 'POSTSCRIPT', 	-command => [ \&BatchExport, 'postscript']  ],
								[ 'command', 'PNG', 		-command => [ \&BatchExport, 'png16m']  ],
								[ 'command', 'PNG ALPHA', 	-command => [ \&BatchExport, 'pngalpha']  ],
								[ 'command', 'JPEG', 		-command => [ \&BatchExport, 'jpeg']  ],
								[ 'command', 'PDF', 		-command => [ \&BatchExport, 'pdfwrite']  ]							
							],
						]																					
					]
				],
				'-',
				[ 'command', 'Print Options ...', -command => \&OptionsPrint,   ],
				[ 'command', 'Print ...',		-command => \&Print, -accelerator => 'Contr+P'  ],
				'-',
				[ 'command', 'Exit',	-command => sub { exit } ],
			]
		],
		[ '~Edit',
			[
				['command', 'Zoom In', 		-command => [ \&Zoom,  1 ] ],
				['command', 'Zoom Out', 	-command => [ \&Zoom, -1 ] ],
				'-',
				['command', 'Center View', 	-command => \&AdjustView, -accelerator => 'F7' ],
				['command', 'Fit View', 	-command => [ \&AdjustView, -fit => 'center' ], -accelerator => 'F8' ],
				'-',
				['command', 'Redraw Ped', -command => \&DrawPed , -accelerator => 'F5'],
				['command', 'Shuffle Haplotype Colors', -command => \&RedrawHaploShuffle , -accelerator => 'F6'],
				'-',
				[ 'checkbutton', ' Show Grid' , -variable => \$grid , -command => \&ShowGrid ]
			]
		],
		[ '~View',
			[
				[ 'cascade', 'Draw Pedigree ...' , -tearoff => 1 ],
			]
		],
		[ '~Options',
			[
				[ 'command', 'Configuration ...', 	-command => \&Configuration ],
				[ 'command', 'Printing ...', -command => \&OptionsPrint  ],
			]
		],
		[ '~Help',
			[
				[ 'command', 'Show Help ...', 		-command => \&ShowHelp  ],
				[ 'command', 'About HaploPainter ...', 	-command => \&ShowAbout ],
			]
		],
	]
}

# some needful functions are chosable via post menu ( right mouse click )
#=================
sub _ContextMenu {
#=================
	[
		['command', 'Zoom In', 	-command => [ \&Zoom,  1, 1 ] ],
		['command', 'Zoom Out', -command => [ \&Zoom, -1, 1 ] ],
		,'-',
		['command', 'Center View', -command => \&AdjustView, -accelerator => 'F7' ],
		['command', 'Fit View', -command => [ \&AdjustView, -fit => 'center' ], -accelerator => 'F8' ],
		,'-',
		[ 'command', 'Configuration ...', 	-command => \&Configuration ],
		[ 'command', 'Print Options ...', -command => \&OptionsPrint,   ],
		,'-',
		[ 'command', 'Save as Postscript', 	-command => [ \&Export, 'postscript' ] ],
		[ 'command', 'Save as JPEG', 		-command => [ \&Export, 'jpeg' ] ],
		[ 'command', 'Save as PNG', 		-command => [ \&Export, 'png16m' ] ],
		[ 'command', 'Save as PNG ALPHA', 	-command => [ \&Export, 'pngalpha']  ],
		[ 'command', 'Save as PDF', 		-command => [ \&Export, 'pdfwrite' ] ],
		,'-',
		[ 'checkbutton', ' Show Grid' , -variable => \$grid , -command => \&ShowGrid ]
	]
}

# 05/2007
# Show Context Menue and store current cursor coordinates 
# for later positioning after zooming
#=====================
sub ShowContextMenue {
#=====================
	my ($c, $menu, $x, $y) = @_;
	$menu->Post($mw->pointerxy); 
	$menu->grabRelease();
	$param->{X} = $x;
	$param->{Y} = $y;	
}


# double clicking uninformative alleles cause appearing dialog box to change
# chromosomal phase or declaring as uninformative
#===============
sub KlickAllel {
#===============
	return if $param->{STATUS};	
	@_ = $param->{ACTIVE_ITEM};
	foreach (@_) {
		if (/ALLEL-(\w)-(\d+)-(.+)/) {

			my $fa = $self->{SID2FATHER}{$3};
			my $mo = $self->{SID2MOTHER}{$3};

			my ($var, $P, $M, $flag);
			my $i = 0;
			my $un = $self->{HAPLO_UNKNOWN_COLOR};

			### Paternaler Haplotyp
			if ($1 eq 'P') {
				$P = $self->{HAPLO}{PID}{$fa}{P}{BAR}[$2][1];
				$M = $self->{HAPLO}{PID}{$fa}{M}{BAR}[$2][1]
			} else {
				$P = $self->{HAPLO}{PID}{$mo}{P}{BAR}[$2][1];
				$M = $self->{HAPLO}{PID}{$mo}{M}{BAR}[$2][1]
			}

			my $d = $mw->DialogBox(-title => 'Set color of Haplotype',-buttons => ['Ok']);
			my $f = $d->Frame(-relief => 'groove', -borderwidth => 2)->pack( -padx => 5, -pady => 5, -expand => 1, -fill => 'both');

			$f->Label(-width => 2, -bg =>  $P)->grid( -row => 0, -column => 0, -sticky => 'e');
			$f->Label(-width => 2, -bg =>  $M)->grid( -row => 1, -column => 0, -sticky => 'e');
			$f->Label(-width => 2, -bg => $un)->grid( -row => 2, -column => 0, -sticky => 'e');

			foreach my $l ( "Paternal", "Maternal", 'Not informative') {
				$f->Radiobutton( -text => $l, -variable => \$var,-value => $l, -command => sub {
					if ($var eq 'Paternal') {
						$self->{HAPLO}{PID}{$3}{$1}{BAR}[$2][1] = $P; $flag = 1;
						$self->{HAPLO}{PID}{$3}{$1}{BAR}[$2][0] = 'NI-3';
					}
					elsif ($var eq 'Maternal') {
						$self->{HAPLO}{PID}{$3}{$1}{BAR}[$2][1] = $M; $flag = 1;
						$self->{HAPLO}{PID}{$3}{$1}{BAR}[$2][0] = 'NI-3';
					} else {
						$self->{HAPLO}{PID}{$3}{$1}{BAR}[$2][0] = 'NI-2'; $flag = 1
					}
				})->grid( -row => $i, -column => 1, -sticky => 'w');
				$i++
			}

			$d->Show();
			if ($flag) {
				FillCanvas();
				SetLines();
				DrawLines();
				DrawHaplo();
			}
		}
	}
}

#================
sub KlickSymbol {
#================
	return if $param->{STATUS};
	my ($c) = @_;	
	@_ = $c->itemcget('current', -tags);
	foreach my $tag (@_) {
		if ($tag =~ /SYM-(\S+)/) {
			my $id = $1;
			
			my $d = $mw->DialogBox(-title => 'Change sample values',-buttons => ['Ok']);
			my $f = $d->Frame(-relief => 'groove', -borderwidth => 2)->pack( -padx => 5, -pady => 5, -expand => 1, -fill => 'both');
			
			$f->Radiobutton(-value => 2 ,-variable =>\$self->{SID2AFF}{$id},-text => 'affected')->grid(-row => 0, -column => 0, -sticky => 'w');
			$f->Radiobutton(-value => 1 ,-variable =>\$self->{SID2AFF}{$id},-text => 'not affected')->grid(-row => 1, -column => 0, -sticky => 'w');
			$f->Radiobutton(-value => 0 ,-variable =>\$self->{SID2AFF}{$id},-text => 'unknown')->grid(-row => 2, -column => 0, -sticky => 'w');
			
			$f->Radiobutton(-value => 1 ,-variable =>\$self->{SID2ALIVE}{$id},-text => 'alive')->grid(-row => 0, -column => 1, -sticky => 'w');
			$f->Radiobutton(-value => 0 ,-variable =>\$self->{SID2ALIVE}{$id},-text => 'not alive')->grid(-row => 1, -column => 1, -sticky => 'w');
			
			$d->Show;
			RedrawPed();
		}
	}
}


# Moving mouse over uninformative alleles from non-founder cause changing its color to red
#===============
sub EnterAllel {
#===============	
	my ($c) = @_;
	my $z = $self->{ZOOM};
	
	@_ = $c->itemcget('current', -tags);
		
	foreach my $tag (@_) {
		if ($tag =~ /ALLEL-(\w)-(\d+)-(.+)/) {
			my $fa = $self->{SID2FATHER}{$3};
			my $mo = $self->{SID2MOTHER}{$3};
			return if ! $fa || ! $mo;

			my ($a1, $a2);
			if ($1 eq 'P') {
				$a1 = $self->{HAPLO}{PID}{$fa}{P}{TEXT}[$2];
				$a2 = $self->{HAPLO}{PID}{$fa}{M}{TEXT}[$2]
			} else {
				$a1 = $self->{HAPLO}{PID}{$mo}{P}{TEXT}[$2];
				$a2 = $self->{HAPLO}{PID}{$mo}{M}{TEXT}[$2]
			}
			
			$param->{ACTIVE_ITEM} = $tag;
			$param->{ACTIVE_COLOUR} = $c->itemcget($tag, -fill);
			
			if ( (! $a1 || ! $a2) || ( $a1 == $a2 ) ) {	
				$c->itemconfigure($tag, -fill => 'red');
			}
		}
	}
}


#==================
sub HeadB1Release {
#==================
	my $c = shift;
	my $z = $self->{ZOOM};
	my $gx = $self->{GITTER_X}*$z;
	my $gy = $self->{GITTER_Y}*$z;	
	@_ = $c->coords('HEAD');
	my $X = $self->{TITLE_X} = sprintf ("%1.0f", $_[0]/$gx);
	my $Y = $self->{TITLE_Y} = sprintf ("%1.0f", $_[1]/$gy);
	
	$c->coords('HEAD', $X*$self->{GITTER_X}*$z, $Y*$self->{GITTER_Y}*$z);	
}


# sub for drag and drop symbols features
#===================
sub MouseB1Release {
#===================	
	my $c = $canvas;
	my $m = $self->{MATRIX};
	my $s = $self->{ACT_SYM};
	my $z = $self->{ZOOM};
	my $gx = $self->{GITTER_X}*$z;
	my $gy = $self->{GITTER_Y}*$z;

	$c->configure(-cursor => 'left_ptr');;
	if ($s->{ID_CLONE}) {
		my $p = $s->{PID};
		my ($x1g, $y1g) = ($m->{P2XY}{$p}{X}, $m->{P2XY}{$p}{Y});

		@_ = $c->coords($s->{ID_ORG});
		my ($x1c, $y1c) = ( ($_[0]+$_[2])*0.5 , ($_[1]+$_[3])*0.5 );
		@_ = $c->coords($s->{ID_CLONE});
		my ($x2c, $y2c) = ( ($_[0]+$_[2])*0.5 , ($_[1]+$_[3])*0.5 );

		my $gxd = sprintf ("%1.0f", ($x2c-$x1c)/$gx);
		my $gyd = sprintf ("%1.0f", ($y2c-$y1c)/$gy);

		my ($x2g, $y2g) = ( $x1g+$gxd, $y1g+$gyd );

		if ( $gxd || $gyd ) {
			### somebody at X/Y ?
			if ( ! $m->{YX2P}{$y2g}{$x2g} ) {
				### spouses there and Y shift -> no action
				if ( ! ( keys %{$self->{COUPLE}{$p}} && $y1g != $y2g ) ) {
					delete $m->{YX2P}{$y1g}{$x1g};
					$m->{YX2P}{$y2g}{$x2g} = $p;
					$m->{P2XY}{$p}{X} = $x2g;
					$m->{P2XY}{$p}{Y} = $y2g;
					
					### modify {STRUK} for correct drawing of multiple mates after drag and drop
					my (%X, @S, @D, @D2, %SAVE);
					my ($CG,$CS,$CP) = FindAdress($p);
					if (ref $self->{STRUK}[$CG][$CS][$CP]) {
						foreach my $p (@{ $self->{STRUK}[$CG][$CS][$CP][0] }) {$X{$self->{MATRIX}{P2XY}{$p}{X}} = $p}
						foreach (sort { $a <=> $b } keys %X) { push @S,$X{$_} }					
						if ($#S > 1) {					
							foreach my $p1 (@S) {
								foreach my $p2 (@S) {
									if ($self->{CHILDREN_COUPLE}{$p1}{$p2} && ! $SAVE{$p1}{$p2} && ! $SAVE{$p2}{$p1}) {
										push @D, [ $p1, $p2 ];
										$SAVE{$p1}{$p2} = 1
									}
								}		
							}						
							foreach my $i (0 .. $#S-1) { push @D2, [ $S[$i], $S[$i+1] ] }						
							$self->{STRUK}[$CG][$CS][$CP] = [ \@S, \@D, \@D2 ];
						}
					}					
					FillCanvas();
					SetLines();
					DrawLines();
					DrawHaplo();
				}
			}
		}
		$c->delete($s->{ID_CLONE});
		undef $s->{ID_CLONE};
	}
}

# moving symbols
#================
sub MouseB1Move {
#================	
	my ($c, $x, $y) = @_;

	$x = $c->canvasx($x);
	$y = $c->canvasy($y);

	$c->move($self->{ACT_SYM}{ID_CLONE},$x-$param->{X},$y-$param->{Y});

	$param->{X} = $x;
	$param->{Y} = $y;
}

# moving title 
#=============
sub MoveHead {
#=============
	my ($c, $x, $y) = @_;
	
	$x = $c->canvasx($x);
	$y = $c->canvasy($y);	
	
	$c->move('current',$x-$param->{X},$y-$param->{Y});

	$param->{X} = $x;
	$param->{Y} = $y;			
}


#===================
sub ActivateSymbol {
#===================
	my ($c, $x, $y) = @_;
	
	$x = $c->canvasx($x);
	$y = $c->canvasy($y);
	
	$param->{X} = $x;
	$param->{Y} = $y;						
	
	$c->Tk::focus;

	return unless $c->cget(-cursor) eq 'left_ptr';	
		
	foreach my $t ($c->itemcget('current', -tags)) {	
		if ($t =~ /^SYM-(.+)$/) {
			@_ = ( $c->coords($t),-width => $c->itemcget($t,-width),
				-outline => $c->itemcget($t,-outline), -fill => 'red' );
			my $id; if ($self->{SID2SEX}{$1} == 1) { $id = $c->createRectangle(@_) }
			elsif ($self->{SID2SEX}{$1} == 2) { $id = $c->createOval(@_) }
			else { $id = $c->createPolygon(@_) }
			$self->{ACT_SYM}{PID} = $1;
			$self->{ACT_SYM}{ID_ORG} = $t;
			$self->{ACT_SYM}{ID_CLONE} = $id;
		 	$c->configure(-cursor => 'fleur');
			last
		}
	}
}

# Start drawing one pedigree
#=========
sub DoIt {
#=========	
	my $fam = shift or return;
	MakeSelf($fam);
	Default('restore');
	ProcessFamily()	or return;
	FindLoops();
	LoopBreak();
	FindTop() or return;
	BuildStruk();
	CheckPedigree() or return undef;
	DuplicateHaplotypes();
	ShuffleFounderColors();
	ProcessHaplotypes();
	DrawPed();
	AdjustView(-fit => 'center');							
	1;
}


##########################################################################

# Questions ?
#=============
sub ShowInfo {
#=============	
	my ($info, $type) = @_;
	if ($batch) { print "$info\n" }
	else {
		$mw->messageBox(
			-title => 'Status report', -message => $info,
			-type => 'OK', -icon => $type || 'info'
		)
	}
}



# some pedigree/ data structure consistence checks
#==================
sub CheckPedigree {
#==================	
	my $flag = 1;
	my $s = $self->{STRUK};
	my $cs = 0;
	my %cs = ();
	my @ss = ();

	foreach my $G (@$s) {
		foreach my $S (@$G) {
			foreach my $P (@$S) {
				if ( ref $P ) {
					foreach my $p ( @{$P->[0]} ) {
						if (! $cs{$p} ) { $cs{$p} = 1 } else { push @ss, $p }
						$cs++
					}
				} else {
					if (! $cs{$P} ) { $cs{$P} = 1 } else { push @ss, $P }
					$cs++
				}
			}
		}
	}


	if (@ss) {
		ShowInfo("Error while preocessing pedigree structure: Persons @ss are duplicated", 'error');
		undef $flag;
	}

	my $cp = scalar @{$pedigree{$self->{FAMILY}}};	
	if ($self->{LOOP}{BREAK}) {
		foreach (keys % { $self->{LOOP}{BREAK} }) {
			foreach (keys % { $self->{LOOP}{BREAK}{$_} }) {
				$cp++
			}
		}
	}
	
	
	if ($cs != $cp)  {
		ShowInfo(
			"Error while processing the pedigree !\n$cs:$cp\n" .
			"This pedigree probably will be drawn incorrect !",
			'error'); undef $flag;
	}	
	#return $flag;
	1
}



# This method implements maximal number of trials to find good drawing solutions
# Given values are found empirical working well. The alligning algorhithm still could be improved !
#============
sub DrawPed {
#============
	return unless $self->{FAMILY};
	my $CrossMin = 0;
	my ($save, $flag);
	WHILE:while (1) {
		$flag = 0;
		FOR:for my $n ( 1 .. 35 ) {
			$self->{COUNT} = 0;
			FindTop();
			BuildStruk();
			BuildMatrix();
			until (AlignMatrix()) { $self->{COUNT}++ ; last if $self->{COUNT} > 120 }
			FillCanvas();
			my $c = SetLines();
			$canvas->update();
			unless ($c) {
				undef $self->{TITLE_X};
				FillCanvas();
				DrawLines();
				DrawHaplo();
				last WHILE
			}
			$CrossMin = $c unless $CrossMin;
		
			if ($c < $CrossMin) {
				$CrossMin = $c;
				$save = freeze($self);
				$flag = 1;
				last FOR;
			}
		}
		
		unless ($flag) {
			$self = thaw($save) if $save;
			undef $self->{TITLE_X};
			FillCanvas();
			SetLines();
			DrawLines();
			DrawHaplo();
			last WHILE;
		}
	}
}

# $self will be stored by Storable ...
#=============
sub SaveSelf {
#=============	
	if (shift) {
		$_ = $mw->getSaveFile(
			-initialfile 	=> basename($self->{FILENAME}),
			-defaultextension	=> 'dump',
			-filetypes		=> [
									[ 'All Files',	 '*' ],
									[ 'HaploPainter Files', 'dump' ]
								]
		) or return undef;
		$self->{FILENAME} = $_;
	} else {
		$_ = $self->{FILENAME} or return undef
	}
	store $self, $_;
}

# ... and restored ...
#================
sub RestoreSelf {
#================	
	my $file = $mw->getOpenFile(
		-filetypes		=> [
								[ 'All Files',	 '*' ],
								[ 'HaploPainter Files', 'dump' ]
							]
	) or return undef;

	$self = retrieve($file);
	my $fileref = $menubar->entrycget('View', -menu);
	my $drawref = $fileref->entrycget('Draw Pedigree ...', -menu);
	$drawref->delete(0,'end');
	RedrawPed();
	AdjustView();
}


###  Loops there ? If yes store information for later queries
###  Rewritten 05/2007
#==============
sub FindLoops {
#==============	
	my $s = $self->{STRUK};
	my (%path, $flag, %P, %N, %D, %D1, %D2, %K, %L, %B);
	my $node_cc = 1;
	
	### network for loop detection
	### couples as nodes
	foreach my $node ( keys % { $self->{PARENT_NODE} }) {
		my ($fid, $mid) = @ { $self->{PARENT_NODE}{$node} };
		
		### check for parent nodes
		foreach my $parent ($fid, $mid) {
			if ( defined $self->{SID2FATHER}{$parent} && defined $self->{SID2MOTHER}{$parent} ) {
				my ($fpar, $mpar) = ($self->{SID2FATHER}{$parent}, $self->{SID2MOTHER}{$parent});
				my $parnode = join '__', nsort($fpar, $mpar);
				$N{$node}{$parnode} = 1;
			}
		}
		
		### check for child nodes
		foreach my $child ( keys % { $self->{CHILDREN_COUPLE}{$fid}{$mid} }) {
			if (defined $self->{CHILDREN_COUPLE}{$child}) {
				foreach my $mate (keys % { $self->{CHILDREN_COUPLE}{$child} }) {
					my $parnode = join '__', nsort($child, $mate);
					$N{$node}{$parnode} = 1;					
				}								
			}	
		}
		
		### create joining parent node for multiple mates without shared parents
		foreach my $parent ( keys % { $self->{CHILDREN_COUPLE} } ) {			
			### is this a multiple mate situation?
			if (scalar (keys % { $self->{CHILDREN_COUPLE}{$parent} }) > 1) {				
				### there is no parent node for this set of multiple mate child nodes
				if (! defined $self->{SID2FATHER}{$parent} && ! defined $self->{SID2MOTHER}{$parent} ) {					
					### pseudo node creation to connect joined mates by one parent node
					my @mates = nsort keys % { $self->{CHILDREN_COUPLE}{$parent} };
					#my $node = $parent . '_PSEUDONODE_' . join '__', @mates;
					my $node = 'PSNODE_' . join ('__', (@mates, $parent)) . '_PSNODE';
															
					foreach my $mate (@mates) {
						my $parnode = join '__', nsort($parent, $mate);
						$N{$node}{$parnode} = 1;
						$N{$parnode}{$node} = 1;
					}										
				}				
			}					
		}		
	}
		
	### prepare start tree including root and one further level
	foreach my $node1 (keys %N) {		
		foreach my $node2 (keys % { $N{$node1} }) {
			$path{$node_cc} = [ $node1, $node2 ]; $node_cc++;
		}
	}
			
	### This code evaluates all loops, clock/anticlock 
	### at every start position inside the loop
	W:while (!$flag) {
		$flag = 1;
		foreach my $p (keys %path) {
			my $r = $path{$p};
			my @plist = @$r;			
			next if $r->[-1] eq 'LOOP';
			
			### delete this path and substitute it by child pathes next in code
			### If there is no path to subsitute it is removed by the way											
			delete $path{$p};
			
			### spacial case inter sibling mate 
			my ($pid1, $pid2) = split '__', $r->[-1];					
			### both sibling and halfsibling mates (may be better handle as separate cases)
			if (defined $self->{SID2FATHER}{$pid1} && defined $self->{SID2FATHER}{$pid2} &&
				defined $self->{SID2MOTHER}{$pid1} && defined $self->{SID2MOTHER}{$pid2}) {
				if (($self->{SID2FATHER}{$pid1} eq $self->{SID2FATHER}{$pid2}) or 
					($self->{SID2MOTHER}{$pid1} eq $self->{SID2MOTHER}{$pid2})) {
					$path{$node_cc} = [ @plist, 'LOOP' ];  $node_cc++; next W
						
				}
			}
			
			### there is only one way back --> delete this path
			my @subnodes = keys % { $N{ $r->[-1] } };
			next if scalar @subnodes == 1;
			
			### look for subnodes
			F:foreach my $node (@subnodes) {				
				### dont go back inside the path!
				next if $node eq $plist[-2];																	
				### unperfect LOOP --> no further processing
				for ( 1 .. scalar @plist-1 ) { next F if $plist[$_] eq $node }				
				### perfekter LOOP ( start = end)
				if ($node eq $plist[0]) { $path{$node_cc} = [ @plist, 'LOOP' ];  $node_cc++  }		
				### expand paths by subnodes else				
				else { 										
					$path{$node_cc} = [ @plist, $node ]; $node_cc++; undef $flag 
				}												  												
			}					 			
		}		
	}	
	
	### processing paths to find duplicates
	foreach my $node (keys %path) {		
		@_ = ();
		foreach (@ {$path{$node}}) {
			### remove LOOP-end tag and pseudonodes
			next if /LOOP|PSNODE/;
			push @_, $_;
		}			
		$_ = join '___', nsort(@_);
		$D{$_} = [ @_ ];
		foreach my $e (@_) { $D1{$_}{$e} = 1 }
	}
	
	
	### return if no loops there
	return unless keys %D;
				
	## if a small loop is part of a bigger loop store this information	
	foreach my $loop1 (keys %D1) {
		foreach my $loop2 (keys %D1) {
			next if $loop1 eq $loop2;
			my ($lp1, $lp2);
			if (  (scalar keys % { $D1{$loop1} }) <  (scalar keys % { $D1{$loop2} }) ) {
				($lp1, $lp2) = ($loop1, $loop2);				
			}			
			elsif (  (scalar keys % { $D1{$loop1} }) >  (scalar keys % { $D1{$loop2} }) ) {
				($lp1, $lp2) = ($loop2, $loop1);				
			}			
			if ($lp1) {
				my $flag;
				foreach my $k (keys % { $D1{$lp1} }) { $flag = 1 if ! $D1{$lp2}{$k} }													
				$D2{$lp2} = 1 if ! $flag								
			}
		}		
	}		
		
	### analyse loop structure
	### find start, middle and end nodes/individuals
	### start nodes/individuals have no parent node but children nodes
	### middle nodes have start and end nodes
	### end nodes have no children but parent nodes	
	my $countl = 0;	
	foreach my $loop (keys %D) {
		my $cansang_flag = 1;
		my %start_nodes;
		$countl++;				 	
		my @loop_list = @ {$D{$loop}};
		my %E;		
						
		### build Hash for every individual inside the loop 
		foreach my $couple (@loop_list) {
			foreach my $pid (split '__', $couple) { 				 
				$E{$pid} = 1;																
			}		
		}		
		
		### exploring loop
		my @node_types;
		foreach my $node (@loop_list) {
			my ($p1, $p2) = (split '__', $node);
						
			### there is a chance that this is a multiple mate case and
			### one of that mate is further connected
			
			### getting all connected mates of this node which are part of the loop
			my %P = ( $p1, 1, $p2, 1);			
			W:while (1) {
				undef $flag;
				foreach my $p ( keys %P ) {
					foreach my $c ( keys % { $self->{COUPLE}{$p} }) {
						if (! $P{$c} && $E{$c}) { $P{$c} = 1; $flag = 1 }
					}
				}
				last W unless $flag
			}
			
			my ($no_start_flag, $no_end_flag) = (0,0);
			foreach my $p (keys %P) {
				### this cannot be a start node
				if ($self->{SID2FATHER}{$p} && $E{$self->{SID2FATHER}{$p}})  { $no_start_flag = 1 }
				if ($self->{SID2MOTHER}{$p} && $E{$self->{SID2MOTHER}{$p}})  { $no_start_flag = 1 }
				
				### this cannot be a end node
				foreach my $p1 (keys %P) {
					foreach my $p2 (keys %P) {
						if ($self->{CHILDREN_COUPLE}{$p1}{$p2}) {
							foreach my $child (keys %{$self->{CHILDREN_COUPLE}{$p1}{$p2}}) {
								if ($E{$child}) { $no_end_flag = 1 }
							}						
						}
					}		
				}						
			}
			
			
			### START nodes
			if (! $no_start_flag && $no_end_flag) {
				$self->{LOOP}{START}{$p1}{$p2} = 1;
				$self->{LOOP}{START}{$p2}{$p1} = 1;											
				$self->{LOOP}{NR2START}{$countl}{$p1} = 1;
				$self->{LOOP}{NR2START}{$countl}{$p2} = 1;
				if ( (scalar keys %P) > 2 ) { 
					push @node_types, 'SM';
					$cansang_flag = 0 if (scalar keys %P) > 3;
				}
				else { push @node_types, 'S_' }
			}
			
			### END nodes
			elsif ( $no_start_flag && ! $no_end_flag) {
				$self->{LOOP}{END}{$p1}{$p2} = 1;
				$self->{LOOP}{END}{$p2}{$p1} = 1;				
				$self->{LOOP}{NR2END}{$countl}{$p1} = 1;
				$self->{LOOP}{NR2END}{$countl}{$p2} = 1;
				if ( (scalar keys %P) > 2 ) { 
					push @node_types, 'EM';
					$cansang_flag = 0; 
				}
				else { push @node_types, 'E_' }			
			}
			
			### MIDDLE nodes
			elsif ( $no_start_flag && $no_end_flag) {
				$self->{LOOP}{MIDDLE}{$p1}{$p2} = 1;
				$self->{LOOP}{MIDDLE}{$p2}{$p1} = 1;				
				$self->{LOOP}{NR2MIDDLE}{$countl}{$p1} = 1;
				$self->{LOOP}{NR2MIDDLE}{$countl}{$p2} = 1;
				if ( (scalar keys %P) > 2 ) { 
					push @node_types, 'MM';
					$cansang_flag = 0; 
				}
				else { push @node_types, 'M_' }	
			}						
		}
				
		### this is a hard to draw loop (found no end nodes in it)  --> mark proper nodes as consanguine
		if (! defined $self->{LOOP}{NR2END}{$countl}) {
			my %R;
			foreach my $node (@loop_list) {
				foreach my $p (split '__', $node) {
					if ($R{$p}) {																							
						foreach my $mate (keys % { $self->{COUPLE}{$p} }) {
							if ($E{$mate}) {							
								if ($self->{SID2MOTHER}{$mate} && $E{$self->{SID2MOTHER}{$mate}} && $self->{SID2FATHER}{$mate} && $E{$self->{SID2FATHER}{$mate}}) {														
									$self->{LOOP}{CONSANGUINE}{$p}{$mate} = 1;
									$self->{LOOP}{CONSANGUINE}{$mate}{$p} = 1;								
								}								
							}							
						}
					}
					$R{$p}++;
				}				
			}
			
			### store those nodes including individuals occuring more then one time in all nodes from the loop
			### such individuals are candidates for breaking a loop
			foreach my $p1 (keys %R) {
				if ($R{$p1}>1) {
					foreach my $node (@loop_list) {
						foreach my $p2 (split '__', $node) {
							if ($p1 eq $p2) {
								$B{PID}{$p1}{$node} = 1;		
							}
						}
					}										
				}
			}																								
		}			
		
		### store consanguine information
		if ($cansang_flag) {
			my ($p1, $p2) = keys % { $self->{LOOP}{NR2END}{$countl} };
			$self->{LOOP}{CONSANGUINE}{$p1}{$p2} = 1;
			$self->{LOOP}{CONSANGUINE}{$p2}{$p1} = 1;
		}
		
		### detection of "asymetric" loops
		### When loops are not in balance - that means that there are more middle nodes on the one side
		### then the other, the end note person which belongs to the side with lower middle notes 
		### must be prevented to draw in the middle part.		
		### It also has to be explored, if middle nodes belong to a multiple mate group (they count as one node together)				
		
		### loop twice over loop_list 
		### exploring number of middle nodes from START --> END
		my $cll = scalar @loop_list;
		my $cc1 = 0;
		my $i1;
		undef $flag;
		for my $i ( -$cll .. $cll-1 ) {
			if ($node_types[$i] =~ /S./) { $flag = 1; next }
			next unless $flag;      
			
			if ($node_types[$i] =~ /E./) {
				$i1 = $i;
				last
			}
			if ( (($node_types[$i] =~ /MM/) && ($node_types[$i-1] !~ /MM/)) or  ($node_types[$i] =~ /M_/)) {
				$cc1++;      	
			} 			    			
		}				
		
		### loop twice over loop_list 
		### exploring number of middle nodes from END --> START
		my $cc2 = 0;
		my $i2;
		undef $flag;
		for my $i ( -$cll .. $cll-1 ) {
			if ($node_types[$i] =~ /E./) { $flag = 1; next }
			next unless $flag;      
			if ($node_types[$i] =~ /S./) {
				$i2 = $i;
				last
			}	

			if ( (($node_types[$i] =~ /MM/) && ($node_types[$i-1] !~ /MM/)) or  ($node_types[$i] =~ /M_/)) {
				$cc2++;      	
			} 			    			
		}
		
		##### asymetric loop !!!
		if ( ($cc1 != $cc2) &&  ! $D2{$loop} ) {
			my ($n1, $n2);
			for my $i ( 1-$cll .. $cll-1 ) {			
				if ( 	($cc1 < $cc2) && ($node_types[$i] =~ /E./) && ($node_types[$i-1] =~ /M.|S./) ) {
					($n1, $n2) = @loop_list[$i-1, $i];					
				}												
				elsif ( ($cc1 > $cc2) && ($node_types[$i] =~ /M.|S./) && ($node_types[$i-1] =~ /E./) ) {
					($n1, $n2) = @loop_list[$i, $i-1];
				}				
			}
			my ($p1, $p2) = split '__', $n1;
			my ($p3, $p4) = split '__', $n2;					
			if ( defined $self->{CHILDREN_COUPLE}{$p1}{$p2}{$p3} ) {
					$self->{LOOP}{DROP_CHILDREN_FROM}{$p3} = 1
			} 
			if ( defined $self->{CHILDREN_COUPLE}{$p1}{$p2}{$p4} ) {
				$self->{LOOP}{DROP_CHILDREN_FROM}{$p4} = 1
			} 										
		}			
	}
	
	### some nodes could be common part of multiple loops
	### such nodes should be get off from individual duplication to prevent trouble
	foreach my $p ( keys % { $B{PID} } ) {
		foreach my $node ( keys % { $B{PID}{$p} } ) {
			$B{NODE}{$node}++;		
		}				
	}
	
	### mark indviduals for loop breaking in case of hard core loops
	if (keys %B) {
		foreach my $p ( keys % { $B{PID} } ) {
			my (@nodes, $flag);
			foreach my $node (keys % { $B{PID}{$p} }) {
				if ($B{NODE}{$node} == 1) { push @nodes, $node }
				else {$flag = 1}
			}
			ChangeOrder(\@nodes);
			shift @nodes if ! $flag;			
			foreach (@nodes) {
				my ($p1, $p2) = split '__', $_;
				if ($p eq $p1) {$self->{LOOP}{BREAK}{$p}{$p2} = 1} 
				else { $self->{LOOP}{BREAK}{$p}{$p1} = 1 }
			}						
		}
	}
	$self->{LOOP_COUNT} = $countl;
}


# Read and process haplotype information from different sources
#==============
sub ReadHaplo {
#==============	
	my (%arg) = @_;
	open (FH, "<" , $arg{-file}) or (ShowInfo("$!: $arg{-file}", 'warning'), return );
		my @file = (<FH>);
	close FH;

	unless (@file) { ShowInfo("$arg{-file} has no data !", 'warning'); return undef }
	my $h1;
	### SIMWALK -> one family - one file
	if ($arg{-format} eq 'SIMWALK') {
		my $fam;
		for (my $i = 0; $i < $#file,; $i++) {
			$_ = $file[$i];
			if (/The results for the pedigree named: (\S+)/) {
				$fam = $1;
				undef $haplo{$fam};
				$h1 = $haplo{$fam}{PID} = {};
			}
			if (/^M /) {
				unless ($fam) {
					ShowInfo("Having problems in finding family name !", 'error'); return undef
				}
				my ($M, $z, $P) = ($_,$file[++$i], $file[++$i] );
				my ($pid, $haplo);
				if ( ($pid, $haplo) = $M =~ /^M (\S+).+\s{7}([0-9@].+[0-9@])\s+$/) {
					$h1->{"$pid"}{M}{TEXT} = [ split ' ', $haplo ];
					s/\@/$self->{HAPLO_UNKNOWN}/ foreach @{$h1->{"$pid"}{M}{TEXT}};
				} else {
					ShowInfo("Having problems in finding maternal haplotype in line\n$M", 'error'); return undef
				}
				if ( ($pid, $haplo) = $P =~ /^P (\S+).+\s{7}([0-9@].+[0-9@])\s+$/) {
					$h1->{"$pid"}{P}{TEXT} = [ split ' ', $haplo ];
					s/\@/$self->{HAPLO_UNKNOWN}/ foreach @{$h1->{"$pid"}{P}{TEXT}};
				} else {
					ShowInfo("Having problems in finding paternal haplotype in line\n$P", 'error'); return undef
				}
			}
		}
	}

 	### GENEHUNTER -> (easy !)
	elsif ($arg{-format} eq 'GENEHUNTER') {
		my $fam; if ($file[0] =~ /^\*+\s+(\S+)/) {
			$fam = $1;
			$h1 = $haplo{$fam}{PID} = {};
		} else {
			ShowInfo("Having problems in finding family name in 1. line\n$file[0]", 'error'); return undef;
		}
		for (my $i = 1; $i < $#file,; $i++) {
			$_ = $file[$i];
			if (/^\*+\s+(\S+)/) {
				$fam = $1 ;
				$h1 = $haplo{$fam}{PID} = {};
				next;
			}
			my ($P, $M) = ($_,$file[++$i]);
			my ($pid, undef, undef, undef, $PH)  = split "\t", $P;
			$h1->{$pid}{P}{TEXT} = [ split ' ', $PH ];
			foreach (@{$h1->{$pid}{P}{TEXT}}) { s/0/$self->{HAPLO_UNKNOWN}/ if $_ eq '0' }
			$M =~ s/\t//g;
			$h1->{$pid}{M}{TEXT} = [ split ' ', $M ];
			foreach (@{$h1->{$pid}{M}{TEXT}}) { s/0/$self->{HAPLO_UNKNOWN}/ if $_ eq '0' }
		}
	}

	### MERLIN -> tricky
	elsif ($arg{-format} eq 'MERLIN') {
		I:for (my $i = 0; $i < $#file; $i++) {
			$_ = $file[$i];

			next unless /^FAMILY\s+(\S+)\s+\[(.+)\]/;
			next if $2 eq 'Uninformative';
			$h1 = $haplo{$1}{PID} = {};
			$i++;
			J:for (my $j = $i; $j < $#file; $j++) {
				$_ = $file[$j];
				chomp;
				if (/^FAMILY/) { $i = $j-1; next I	}
				next unless $_;
				if ($_) {
					### aus erster Zeile Personen extrahieren
					my @pid = split ' ', $_;
					my @p; for ( my $k = 0; $k < $#pid; $k+=2 ) {
						push @p,$pid[$k];
						$h1->{$pid[$k]}{M}{TEXT} = [];
						$h1->{$pid[$k]}{P}{TEXT} = [];
					}
					### aus naechsten Zeilen Haplotypen isolieren
					L:for (my $l = $j+1; $l < $#file; $l++) {
						$_ = $file[$l];
						chomp;
						unless ($_) { $j = $l; next J }
						my @L = split;
						for (my $m = 0; $m <= $#p; $m++ ) {
							my $pid = $p[$m];
							my $z = $L[$m*3]; if ($z =~ /,/) { @_ = split ',',$z; $z = $_[0] };
							$z =~ s/[A-Za-z]//g;
							$z = $self->{HAPLO_UNKNOWN} if $z eq '?';
							push @{$h1->{$pid}{M}{TEXT}}, $z;
							my $g = $L[($m*3)+2]; if ($g =~ /,/) { @_ = split ',',$g; $g = $_[0] };
							$g =~ s/[A-Za-z]//g;
							$g = $self->{HAPLO_UNKNOWN} if $g eq '?';
							push @{$h1->{$pid}{P}{TEXT}}, $g;
						}
					}
				}
			}
		}
	}
	### ALLEGRO (thank you !)
	elsif ($arg{-format} eq 'ALLEGRO') {
		foreach (@file) { @_ = split; undef $haplo{$_[0]} if $_[0] }
		for (my $i = 0; $i < $#file; $i++) {
			$_ = $file[$i];
			chomp;
			next unless $_;
			next if /^       /;
			@_ = split; 
			next unless @_;
			$haplo{$_[0]}{PID}{$_[1]}{P}{TEXT} = [ @_[ 6 .. $#_] ];
			@_ = split ' ', $file[++$i]; 
			next unless @_;
			$haplo{$_[0]}{PID}{$_[1]}{M}{TEXT} = [ @_[ 6 .. $#_] ];
		}
	}

	else { ShowInfo ("Unknown haplotype file format $arg{-format} !", 'info') ; return undef }

	### produce 'dummy map' when haplotype information are loaded
	### this is replaced later when 'real' map files come in
	foreach my $fam ( keys %haplo ) {
		(my $pid) = keys %{ $haplo{$fam}{PID} } or next;
		if ( $haplo{$fam}{PID}{$pid}{P}{TEXT} ) {
			for my $i ( 0 .. $#{ $haplo{$fam}{PID}{$pid}{P}{TEXT} } ) {
				$haplo{$fam}{DRAW}[$i] = 1;
				$map{MARKER}[$i] = 'Marker' . sprintf("%02.0f",$i+1) unless $map{MARKER}[$i];
			}
		}
	}	
	$param->{HAPLO_PATH} = $arg{-file};
	1;
}

### Loop breaking adds new individuals. 
### Haplotypes have to be duplicated thus
#========================
sub DuplicateHaplotypes {
#========================
	###in case of duplicated PIDs copy the haplotype information
	foreach my $fam ( keys %haplo ) {
		foreach my $pid ( keys %{ $haplo{$fam}{PID} } ) {	
			if ($self->{DUPLICATED_PID}{$pid}) { 
				foreach my $pid_n (keys % { $self->{DUPLICATED_PID}{$pid} }) {
					$haplo{$fam}{PID}{$pid_n}{P}{TEXT}  = [ @ { $haplo{$fam}{PID}{$pid}{P}{TEXT} } ];
					$haplo{$fam}{PID}{$pid_n}{'M'}{TEXT}  = [ @ { $haplo{$fam}{PID}{$pid}{'M'}{TEXT} } ];
				}
			}
		}		
	}		
}


# Read map files 
#============
sub ReadMap {
#============	
	my (%arg) = @_;
	if ($arg{-file}) {
		open (FHM, "<" , $arg{-file}) or ShowInfo("$! $arg{-file}",'warning') && return;
			while (<FHM>) { ${$arg{-data}} .= $_ } 
		close FHM;
	}
	unless ($arg{-data}) { ShowInfo("No data to read !", 'warning'); return undef }

	my $sc; if ( $map{MARKER} && @{$map{MARKER}}) { $sc = scalar @{$map{MARKER}} }
	%map = ();

	### CHR-POS-MARKER Format
	if ($arg{-format} eq '1') {
		my $i = 0; foreach (split "\n", ${$arg{-data}}) {
			next unless $_;
			s/\t/ /g;
			next if /^#|\*|!|CHR/i;
			my ($chr, $pos, $marker) =  split ' ', $_;
			next if ( ! $chr || ! defined $pos || ! $marker );
			$map{CHROMOSOM} = $chr;
			$map{POS}[$i] = $pos;
			$map{MARKER}[$i] = $marker;
			$i++;
		}
	}
	
	### CHR-MARKER-POS
	elsif ($arg{-format} eq '2') {
		my $i = 0; foreach (split "\n", ${$arg{-data}}) {
			next unless $_;
			s/\t/ /g;
			next if /^#|\*|!|CHR/i;
			my ($chr, $marker, $pos) =  split ' ', $_;
			next if ( ! $chr || ! defined $pos || ! $marker );
			$map{CHROMOSOM} = $chr;
			$map{POS}[$i] = $pos;
			$map{MARKER}[$i] = $marker;
			$i++;
		}
	}
	
	### catch wrong positions, converting of , --> . 
	foreach ( @ { $map{POS} } ) {
		s/,/\./g;
		if (/[^-+.0-9]/) {
			ShowInfo("One ore more marker positions are corrupted!\n$_",'warning');
			%map = ();
			return undef;
		}
	}
	
	my $sm = scalar @{$map{MARKER}};
	if ( $sc && ($sc != $sm) ) {
		ShowInfo("This map file ($sm) consists of more or less marker then have been loaded from the haplotype file ($sc)!",'warning');
		%map = ();
		for (0 .. $sc-1) { $map{MARKER}[$_] = 'Marker' . sprintf("%02.0f",$_+1)  }
	}
	1;
}

# Reading pedigree information
#============
sub ReadPed {
#============	
	my (%arg) = @_;
	if ($arg{-file}) {
		open (FH, "<" , $arg{-file}) or (ShowInfo("$! $arg{-file}",'warning'), return undef);
			while (<FH>) { ${$arg{-data}} .= $_ }  ### -file wird in -data ueberfuehrt
		close FH;
	} 
	ShowInfo("File $arg{-file} is emty !", 'warning') unless $arg{-data};

	#########################################################################
	### Step 1 : read PedData in ARRAY
	#########################################################################

	%pedigree = ();
	%haplo = ();
	%map = ();
	undef $self->{HAPLO};
	

	### PRAEMAKEPED format --- delimiter = white space; fields = 6
	if ( uc $arg{-format} eq 'PRAEMAKEPED' ) {
		foreach (split "\n", ${$arg{-data}}) {
			next unless $_;
			next if /^#|\*|!/;
			my @line =  split " ",$_;
			next unless @line;
			my $fam = shift @line;
			next unless $fam;
			push @{ $pedigree{$fam} }, \@line;
		}
	}

	### PRAEMAKEPED_PLUS format --- delimiter = tab key; fields = unlimited
	elsif ( uc $arg{-format} eq 'PRAEMAKEPED_PLUS') {
		foreach (split "\n", ${$arg{-data}}) {
			next unless $_;
			next if /^#|\*|!/;
			my @line =  split "\t", $_;
			next unless @line;
			my $fam = shift @line;
			next unless $fam;
			push @{ $pedigree{$fam} }, \@line;
		}
	}
	

	### POSTMAKEPED format  --- delimiter = white space; fields = 6
	elsif (uc $arg{-format} eq 'POSTMAKEPED') {
		foreach (split "\n", ${$arg{-data}}) {
			next unless $_;
			next if /^#|\*|!/;
			@_ =  split " ",$_;
			next unless @_;
			push @{ $pedigree{$_[0]} }, [ @_[ 1..3, 7, 9 .. $#_-4  ] ]
			
		}			
	} 

	unless (%pedigree) {
		ShowInfo("There are no data to read !", 'warning'); return undef
	}
	$param->{PEDIGREE_PATH} = $arg{-file};
	$param->{PEDIGREE_FORMAT} = $arg{-format};
	undef $param->{HAPLO_PATH};
	
	1;
}



#########################################################################
### Step 2: Read PedData $struk
#########################################################################

#==================
sub ProcessFamily {
#==================	
	my $fam = $self->{FAMILY};
	my $line_error;
	### allowed SEX values
	my %sex = (0,1,1,1,2,1);
	### allowed affection status values
	my %aff = (0,1,1,1,2,1);
	
	unless ($pedigree{$fam}) { ShowInfo("There is no family $fam ???",'error'); return undef }
	
	foreach my $l (@{$pedigree{$fam}}) {
		next unless @$l;
		my ($sid, $fid, $mid, $sex, $aff, $livestat, @sample_info) = @$l;
		foreach (@$l) { $_ = '' unless defined $_ }
		if (! $sid || ! defined $fid || ! defined $mid) {
			$line_error .= "Error in line: @$l\n"; next
		}

		if ( ! defined $sex || ! defined $sex{$sex} ) {
			$line_error .= "Unknown Sex in line: @$l\n"; $sex = 0
		}
		if ( ! defined $aff || ! defined $aff{$aff} ) {
			$line_error .= "Unknown Aff status in line: @$l\n"; $aff = 0
		}

		if ($fid && $mid) {

			### Vater + Mutter jeder Person
			$self->{SID2FATHER}{$sid} = $fid;
			$self->{SID2MOTHER}{$sid} = $mid;

			### Kinder der Personen
			$self->{CHILDREN}{$fid}{$sid} = 1;
			$self->{CHILDREN}{$mid}{$sid} = 1;

			### Kinder des Paares
			$self->{CHILDREN_COUPLE}{$fid}{$mid}{$sid} = 1;
			$self->{CHILDREN_COUPLE}{$mid}{$fid}{$sid} = 1;

			### Partner der Person
			$self->{COUPLE}{$fid}{$mid} = 1;
			$self->{COUPLE}{$mid}{$fid} = 1;
			
			### parent node creation
			$_ = join '__', nsort($fid,$mid);
			$self->{PARENT_NODE}{$_} = [$fid,$mid];
			
		}

		### ( bzw FOUNDER Status )
		elsif ( ! $fid && ! $mid  )  { $self->{FOUNDER}{$sid} = 1 }
		else { $line_error .= "Error in line - father or mother is zero: @$l\n"; next }

		### individuals gender
		$self->{SID2SEX}{$sid} = $sex;

		### individuals affection status
		$self->{SID2AFF}{$sid} = $aff;

		### sibs and mates
		if ($fid) { $self->{SIBS}{$fid . '__' . $mid}{$sid} = 1 }
		
		### Sample ID
		$self->{PID}{$sid} = 1;
		
		### Case Information 1. Row = SampleID
		$self->{CASE_INFO}{PID}{$sid}{'Case_Info_1'} = $sid;
		$self->{CASE_INFO}{COL_TO_NAME}{1} = 'Case_Info_1' ;
		$self->{CASE_INFO}{COL_NAMES}{Case_Info_1} = 1;
		
		
		### live status, if there
		if ( ($param->{PEDIGREE_FORMAT} eq 'PRAEMAKEPED_PLUS') and (defined $livestat) ) {
			if ($livestat) { $self->{SID2ALIVE}{$sid} = 1 } else { $self->{SID2ALIVE}{$sid} = 0 }			
		} else {
			$self->{SID2ALIVE}{$sid} = 1
		}
		
		### case info, if there
		if ( ($param->{PEDIGREE_FORMAT} eq 'PRAEMAKEPED_PLUS') and (@sample_info) ) {
			my $nr = scalar @sample_info;
			for (my $i=0; $i < scalar @sample_info; $i++) {
				if (defined $sample_info[$i] && ($sample_info[$i] ne '') ) {
					my $name = 'Case_Info_' . +($i+2);
					$self->{CASE_INFO}{PID}{$sid}{$name} = $sample_info[$i];
					$self->{CASE_INFO}{COL_TO_NAME}{$i+2} = $name  if $i<3;
					$self->{CASE_INFO}{COL_NAMES}{$name} = 1;
				}
			}									
		}							
	}

	if ($line_error) { ShowInfo("There are errors in this pedfile !\n$line_error", 'error'); return undef }

	### some checks ...
	### 1. gender of parents
	my $er;
	foreach my $sid ( keys % { $self->{SID2FATHER} } ) {
		$_ = $self->{SID2FATHER}{$sid};
		$er .= "Sex of $_ should be 1 cause of declaration as father of $sid.\n" if $self->{SID2SEX}{$_} != 1
	}
	foreach my $sid ( keys % { $self->{SID2MOTHER} } ) {
		$_ = $self->{SID2MOTHER}{$sid};
		$er .= "Sex of $_ should be 2 cause of declaration as mother of $sid.\n" if $self->{SID2SEX}{$_} != 2
	}
	### 2. founder without children
	foreach my $founder ( keys % { $self->{FOUNDER} } ) {
		$er .= "Founder $founder has no children.\n" unless keys %{ $self->{CHILDREN}{$founder} }
	}

	if ($er) {ShowInfo("There are errors in family $fam !\n\n$er", 'error'); return undef }
	1;
}

#==================
sub ShuffleColors {
#==================	
	return unless keys % { $self->{HAPLO}{PID} };
	my %t;
	my %s = ( $self->{HAPLO_UNKNOWN_COLOR} => 1, 'NI-0' => 1, 'NI-1' => 1, 'NI-2' => 1, 'NI-3' => 1 );
	### which colors are there  ?
	foreach my $p (keys %{$self->{PID}}) {
		next unless $self->{HAPLO}{PID}{$p};
		foreach my $mp ( 'M', 'P' ) {
			foreach (@ { $self->{HAPLO}{PID}{$p}{$mp}{BAR} }) {
				$s{@$_[1]} = 1 if $self->{HAPLO}{PID}{$p}{$mp}{SAVE};
				$t{@$_[1]} = @$_[1]
			}
		}
	}

	### make new haplotype colors
	foreach (keys %t) {
		if (! $s{$_} ) {
			$t{$_} = sprintf("#%02x%02x%02x", int(rand(256)),int(rand(256)),int(rand(256)));
		}
	}

	### write back colors
	foreach my $p (keys %{$self->{PID}}) {
		next unless $self->{HAPLO}{PID}{$p};
		foreach my $mp ( 'M', 'P' ) {
			foreach  (@ { $self->{HAPLO}{PID}{$p}{$mp}{BAR} }) {
				@$_[1] = $t{@$_[1]}
			}
		}
	}
}



### codes for genotypes:
### I   : informative genotype
### NI-0: completely lost haplotype
### NI-1: unique lost genotype
### NI-2: genotype OK + 'by hand' declared as non-informative
### NI-3: genotype OK +  automatic declared as non-informative

#=========================
sub ShuffleFounderColors {
#=========================	
	return unless $self->{HAPLO};
	return unless keys %{ $self->{HAPLO}{PID} };
			
	my $h = $self->{HAPLO}{PID};
	my $un = $self->{HAPLO_UNKNOWN};
	my $huc = $self->{HAPLO_UNKNOWN_COLOR};

	my @founder = keys %{$self->{FOUNDER}} or return undef;

	### alle Farbinformationen zu den Bars der Founder loeschen
	### clear all color information of founder bars
	foreach my $pid (keys %{$self->{PID}}) {
		next unless defined $h->{$pid};
		undef $h->{$pid}{M}{BAR} unless $h->{$pid}{M}{SAVE};
		undef $h->{$pid}{P}{BAR} unless $h->{$pid}{P}{SAVE};
	}

	### alle Founder vorbelegen
	### declare founder
	my $c = scalar @{ $self->{HAPLO}{MAP}{MARKER} };
	foreach my $p (@founder) {
		if ( $h->{"$p"} ) {
			foreach my $m ( 'M' , 'P' ) {
				next unless $h->{$p}{$m};
				next if $h->{$p}{$m}{SAVE};   ### diesen Haplotyp nicht anfassen
				$h->{$p}{$m}{HIDE} = 0;
				my $co = sprintf("#%02x%02x%02x", int(rand(256)),int(rand(256)),int(rand(256)));
				my $flag; for ( 1 .. $c ) {
					my $al = $h->{"$p"}{$m}{TEXT}[$_-1];
					if ($al eq $un) {
						push @{$h->{"$p"}{$m}{BAR}}, [ 'NI-1', $co ]
					}
					else {
						push @{$h->{"$p"}{$m}{BAR}}, ['I', $co ] ; $flag = 1
					}
				}
				unless ($flag) {
					foreach (@{$h->{"$p"}{$m}{BAR}}) { @$_[0] = 'NI-0' }
 				}
			}
		}
	}
	1
}

# Processing haplotype blocks
#======================
sub ProcessHaplotypes {
#======================
	return unless $self->{HAPLO};
	return unless $self->{HAPLO}{PID};

	my $h = $self->{HAPLO}{PID};
	my $s = $self->{STRUK};
	
	### delete everything instaed of founder
	foreach my $pid (keys %{$self->{PID} }) {
		next if $self->{FOUNDER}{$pid};
		next unless defined $h->{$pid};
		undef $h->{$pid}{P}{BAR};
		undef $h->{$pid}{M}{BAR};
	}
		
	###  derive haplotype colors
	W:while (1) {
		my $flag = 0;
		F:foreach my $pid (keys %{$self->{PID}}) {
			next if $self->{FOUNDER}{$pid};
			next unless $h->{$pid};
			### still no haplotype derived
			if (! $h->{$pid}{P}{BAR} || ! $h->{$pid}{M}{BAR} ) {
				next if ! $h->{$pid}{M}{TEXT} || ! $h->{$pid}{P}{TEXT};
				
				### duplicate color information from duplicated pids
				if ($self->{DUPLICATED_PID_ORIG}{$pid}) {
					my $orig_pid = $self->{DUPLICATED_PID_ORIG}{$pid};					
					if ($h->{$orig_pid}{P}{BAR} && $h->{$orig_pid}{M}{BAR}) {
						foreach ( @ { $h->{$orig_pid}{P}{BAR} } ) { push @ { $h->{$pid}{P}{BAR} }, [ @$_ ] }
						foreach ( @ { $h->{$orig_pid}{M}{BAR} } ) { push @ { $h->{$pid}{M}{BAR} }, [ @$_ ] }					
						next;
					}
					else { next }										
				}							
				
				my ($p, $m) = ( $self->{SID2FATHER}{$pid}, $self->{SID2MOTHER}{$pid} );
				if ( $h->{$p}{P}{TEXT} && $h->{$p}{M}{TEXT} ) {
					if ( ! $h->{$p}{P}{BAR} || ! $h->{$p}{M}{BAR}) {  $flag = 1 }
					else {
						my $a = $h->{$pid}{P}{TEXT};
						### BARs + ALLELE from father
						my ($aa1, $aa2) = ( $h->{$p}{P}{TEXT}, $h->{$p}{M}{TEXT} );
						my ($ba1, $ba2) = ( $h->{$p}{P}{BAR},  $h->{$p}{M}{BAR} );
						$h->{$pid}{P}{BAR} = CompleteBar($a, $aa1, $ba1, $aa2, $ba2);												
					}
				} else {
					ShowInfo("The file seemes to be corrupted - missing haplotype for $pid ?\n",'error');
					delete $self->{HAPLO};
					delete $haplo{$self->{FAMILY}};
					return undef
					
				}

				if ( $h->{$m}{P}{TEXT} && $h->{$m}{M}{TEXT} ) {
					if (! $h->{$m}{P}{BAR} || ! $h->{$m}{M}{BAR}) {  $flag = 1 }
					else {
						my $b = $h->{$pid}{M}{TEXT};
						### BARs + ALLELE from mother
						my ($ba3, $ba4) = ( $h->{$m}{P}{BAR},  $h->{$m}{M}{BAR} );
						my ($aa3, $aa4) = ( $h->{$m}{P}{TEXT}, $h->{$m}{M}{TEXT} );
						$h->{$pid}{M}{BAR} = CompleteBar($b, $aa3, $ba3, $aa4, $ba4);
					}
				} else {
					ShowInfo("The file seemed to be corrupted - missing haplotype for $m ?\n",'error');

					delete $self->{HAPLO};
					delete $haplo{$self->{FAMILY}};
					return undef
				}
			}
		}
		last W unless $flag;
	}
	1;
}

#================
sub CompleteBar {
#================	
	my ($a, $aa1, $ba1, $aa2, $ba2) = @_;
	return undef if ! $ba1 || ! $ba2 || ! @$ba1 || ! @$ba2;

	my ($phase, @bar);
	my $un = $self->{HAPLO_UNKNOWN};
	my $unc = $self->{HAPLO_UNKNOWN_COLOR};

	### Phase ist nicht definiert -> Vorr�cken bis zur ersten informativen Stelle
	### und Phase danach definieren
	for (my $j = 0; $j < scalar @$a; $j++) {
		next if @$aa1[$j] eq @$aa2[$j];
		if (@$a[$j] eq @$aa1[$j]) { $phase = 1 } else { $phase = 2 } last
	}
	### wenn das fehlschlaegt ist der Ganze Haplotyp fuer die Katz
	unless ($phase) {
		push @bar, [ 'NI-0', $unc ] foreach @$a;
		return \@bar
	}

	for (my $i = 0; $i < scalar @$a; $i++) {
		### nicht informative Stelle -> entweder Haplotyp fortfuehren
		### oder, wenn voreingestellt als uninformativ deklarieren
		if (@$a[$i] eq $un) {
			if    ($phase == 1) { push @bar, [ 'NI-1', $$ba1[$i][1] ]	 }
			elsif ($phase == 2) { push @bar, [ 'NI-1', $$ba2[$i][1] ]	 }
		}
		elsif ( (@$aa1[$i] eq @$aa2[$i])  ) {
			if    ($phase == 1) { push @bar, [ 'NI-3', $$ba1[$i][1] ]	 }
			elsif ($phase == 2) { push @bar, [ 'NI-3', $$ba2[$i][1] ]	 }
		}
		else {
			if (@$a[$i] eq @$aa1[$i]) { push @bar, [ 'I', $$ba1[$i][1] ]; $phase = 1 }
			else { push @bar, [ 'I', $$ba2[$i][1] ]; $phase = 2 }
		}
	}
	return \@bar;
}


# Which founder couple come to the family in which generation ?
# 12/06 STRUK adapted at multiple mate bug
#=============
sub FindTop {
#=============	
	my (%Top, $f, $m, $flag);
			
	P:foreach my $partner ( keys % { $self->{SIBS} } ) {
		($f, $m) = split '__', $partner;
		
		## find everybody joined in couple group  
		## code from SetCouple 05/2007		
		my %P = ( $f => 1, $m => 1);
		W:while (1) {
			undef $flag;
			foreach my $p ( keys %P ) {
				foreach my $c ( keys % { $self->{COUPLE}{$p} }) {
					if (! $P{$c} ) {$P{$c} = 1; $flag = 1}
				}
			}
			last W unless $flag
		}
		
		foreach my $s (keys %P) {
			foreach	(keys % { $self->{COUPLE}{$s} } ) {
				if ( (! $self->{FOUNDER}{$_}) && (! $self->{CHILDREN}{$s}{$_} ) && ! $self->{DUPLICATED_PID_ORIG}{$_} ) {
					next P
				}
			}
		}
		
				
		if ( 
		((defined $self->{FOUNDER}{$f} or (defined $self->{DUPLICATED_PID_ORIG}{$f}))) and 
		((defined $self->{FOUNDER}{$m} or (defined $self->{DUPLICATED_PID_ORIG}{$m})))
		) {
			$Top{$partner} = [ $f, $m ];
			$self->{STRUK} = 	[
									[
										[
											[
												[ $f,$m ],
												[ [$f,$m] ],
												[ [$f,$m] ],
											]
										]
									]
								];

		}
	}
	### are there no founders ? ---> ERROR
	@_ = keys %Top;
	
	if (! @_) {
		 ShowInfo("There is no founder couple in this family !\nFurther drawing aborted.", 'error'); 
		 return undef;
	}

	### Which founder belong to which generation ??

	### If there are more then one founder couple, this method examine with help of BuildStruk()
	### separate sub family structures and search for connecting overlapping peoples
	### In some situations this has been shown to fail, future work !
	if ($#_) {
		my %G2P;
		foreach my $c ( sort keys %Top ) {
			$self->{STRUK} = [
								[
									[
										[
											[ @{$Top{$c}}],
											[$Top{$c} ],
											[$Top{$c} ],
										]
									]
								]
							] ;

			BuildStruk( -modus => 1 );
			my $s = $self->{STRUK};

			### extract persons for each generation
			my $g = 0;
			foreach my $G (@$s) {
				foreach my $S (@$G) {
					foreach my $P (@$S) {
						if ( ref $P ) {
							foreach my $p ( @{$P->[0]} ) { $G2P{$c}{$g}{$p} = 1 }
						} else {  $G2P{$c}{$g}{$P} = 1 }
					}
				} $g++
			}
		}
		
		### find individual intersection and generation relationship
		my %calc;
		C1:foreach my $c1 ( keys %G2P ) {
			foreach my $G1 ( keys %{$G2P{$c1} } ) {
				foreach my $p1 ( keys %{$G2P{$c1}{$G1} } ) {
					C2:foreach my $c2 ( keys %G2P ) {
						next if $c2 eq $c1;
						foreach my $G2 ( keys %{$G2P{$c2} } ) {
							foreach my $p2 ( keys %{$G2P{$c2}{$G2} } ) {
								if ($p1 eq $p2) {
									if (! %calc) {
										$calc{$G1}{$c1} = 1;
										$calc{$G2}{$c2} = 1;
									} else {
										foreach my $g ( keys %calc ) {
											if ($calc{$g}{$c1}) {
												my $diff = $g-$G1;
												$calc{$G2+$diff}{$c2} = 1
											}
											if ($calc{$g}{$c2}) {
												my $diff = $g-$G2;
												$calc{$G1+$diff}{$c1} = 1
											}
										}
									}
									next C2
								}
							}
						}
					}
				}
			}
		}
		
		### declaration of founder/generation
		my %save2;
		my ($max) =  sort { $b <=> $a } keys %calc;
		foreach my $g (sort { $b <=> $a } keys %calc) {
			foreach my $c (keys % { $calc{$g} }) {
				if (! $save2{$c}) {
					$self->{FOUNDER_COUPLE}{$max-$g}{$c} = 1;
					$save2{$c} = 1
				}
			}
		}

		### Sollte eigentlich nicht mehr vorkommen ... aber man weiss ja nie !
		unless ($self->{FOUNDER_COUPLE}{0}) {
			ShowInfo("There is no founder couple in generation 1 !",'error');
			return undef;
		}

		### multiple mates can be cleared ... see method SetCouple()
		my %save;
		foreach my $g ( keys % { $self->{FOUNDER_COUPLE} } ) {
			foreach my $coup ( keys % { $self->{FOUNDER_COUPLE}{$g} } ) {
				my ($p1, $p2) = split '__', $coup;
				if ($save{$p1} or $save{$p2}) {
					delete $self->{FOUNDER_COUPLE}{$g}{$coup};
				} else { $save{$p1} = 1; $save{$p2} = 1 }
					
			}
		}

		### set up founder couples in {STRUK}
		$self->{STRUK} = [[]];
		my $s = $self->{STRUK}[0];
		my @couples = keys % { $self->{FOUNDER_COUPLE}{0} };
		ChangeOrder(\@couples);		
		foreach (@couples) {
			my ($p1, $p2) = split '__', $_;
			my $Next_S = [];
			push @$s, $Next_S;
			if (scalar (keys % { $self->{COUPLE}{$p1} }) > 1) { push @$Next_S, SetCouples($p1) }
			else { push @$Next_S, SetCouples($p2) }
		}
	}
	
	
	1;
}

### change order of elements in an array
### input=output= reference to array
#================
sub ChangeOrder {
#================	
	my $array = shift;
	return if ! $array || ! @$array;
	return if scalar @$array == 1;
	for (my $i = @$array; --$i; ) {
		my $j = int rand ($i+1);
		@$array[$i,$j] = @$array[$j,$i];
	}
}


# ausgehend von Generation X wird Pedigree Struktur neu erstellt
# Reihenfolge der Paare von Generation X sind ausschlaggebend f�r die
# Reihenfolge der Geschwister/Paar-Gruppen der n�chsten Generation
# Important subroutine, building $struk in 'Top to Bottum' strategy
#===============
sub BuildStruk {
#===============	
	my (%arg) = @_;
	my $G = 0;
	my $EndFlag = 1;
	my $s = $self->{STRUK};
	$self->{STORE_DRAWN} = {};
	### clear generation from $G+1
	$#{$s}=0;
	while ($EndFlag) {
		my $Next_G = []; push @$s, $Next_G;
		undef $EndFlag;
		foreach my $S ( @ { $s->[$G] } ) {
			foreach my $P ( @$S ) {
				if ( ref $P ) {
					$EndFlag = 1;
					foreach my $p ( @ { $P->[1] } ) {
						
						my @children; foreach (keys % { $self->{CHILDREN_COUPLE}{@$p[0]}{@$p[1]} }) {
							if (! $self->{LOOP}{DROP_CHILDREN_FROM}{$_} && ! $self->{STORE_DRAWN}{$_} ) {
								push @children, $_								
							}														
						}
												
						ChangeOrder(\@children) if @children;
																										
						my $Next_S = []; if (@children) { push @$Next_G, $Next_S }
						
						foreach my $child (@children) {							
							if ( keys % {$self->{COUPLE}{$child}} ) {								
								push @$Next_S, SetCouples($child) unless  $self->{STORE_DRAWN}{$child};
								foreach ( keys % { $self->{COUPLE}{$child} }, $child ) { $self->{STORE_DRAWN}{$_} = 1 }
							} else {
								push @$Next_S, $child unless  $self->{STORE_DRAWN}{$child};
								$self->{STORE_DRAWN}{$child} = 1
							}
						}
					}
				}
			}
		}
		### if there are founder couples, they have to be integrated in {STRUK} as new starting point
		if ($self->{FOUNDER_COUPLE}{$G+1}) {
			foreach ( keys % { $self->{FOUNDER_COUPLE}{$G+1} } ) {
				my ($p1) = split '__', $_;
				my $Next_S = [];
				if (int(rand(2))) { push @$Next_G, $Next_S } else { unshift  @$Next_G, $Next_S }
				push @$Next_S, SetCouples($p1);
			}
		}
		$G++;
	}
	pop @$s;	
}


# Find 'adress' of person in $self->{STRUK}
#===============
sub FindAdress {
#===============	
	my ($ziel) = @_;
	return undef unless $ziel;
	my $s = $self->{STRUK};
	my $CG = 0; foreach my $G (@$s) {
		my $CS = 0; foreach my $S (@$G) {
			my $CP = 0; foreach my $P (@$S) {
				if ( ref $P ) {
					my $Cp = 0; foreach my $p ( @{$P->[0]} ) {
						if ($ziel eq $p) { return ($CG,$CS,$CP,$Cp)  };
						$Cp++
					}
				} else {
					if ($ziel eq $P) {  return ($CG,$CS,$CP)  }
				} $CP++
			} $CS++
		} $CG++
	}
	return undef;
}

# Zeichen-Matrix anlegen. Von STRUK ausgehend werden die relativen Zeichenpositionen
# aller Personen generationsweise festgelegt (P2XY/YX2P)
# Next layer after {STRUK} is {MATRIX} -> translation into relative XY Positions
#================
sub BuildMatrix {
#================	
	my $s  = $self->{STRUK};
	$self->{MATRIX} = {};
	$self->{PID_SAVE} = {};
	my $mt = $self->{MATRIX};
	my $x = my $x0 = 0;
	my $y = my $y0 = 0;
	my $xs	= $self->{X_SPACE};
	my $ys	= $self->{Y_SPACE};
	
	### Zeichenmatrix anlegen
	foreach my $G (@$s) {
		foreach my $S (@$G) {
			foreach my $P (@$S) {
				if ( ref $P ) {
					foreach my $p ( @{$P->[0]} ) {
						$mt->{P2XY}{$p}   = { X => $x, Y => $y };
						$mt->{YX2P}{$y}{$x} = $p;
						$x+= $xs+1
					}
				} else {
					$mt->{P2XY}{$P}   = { X => $x, Y => $y };
					$mt->{YX2P}{$y}{$x} = $P;
					$x+= $xs+1
				}
			}
		}
		$x = $x0;
		$y+= $ys
	}
}

### new method 05/2007
### basic DBI support
#=======================
sub ImportPedegreeDBI {
#=======================
	
	my $d = $mw->Toplevel(-title => 'Database Connection');						
	my %T = ( qw / DB_TYPE 1 DB_HOST 1 DB_PORT 1 DB_RELATION 1 DB_SID 1 DB_UNAME 1 /);
		
	my $f1 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $f2 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $f3 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $f4 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $f5 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $f6 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $f7 = $d->Frame()->pack(-side => 'top',  -anchor => 'w', -fill => 'x');
	my $fb = $d->Frame()->pack(-side => 'top', -padx => 5, -pady => 5,  -fill => 'x');
	
	### Connect bottom
	$fb->Button(-text => 'Connect', -width => 10, -command => sub {
		foreach ( qw/ DB_TYPE DB_HOST DB_PORT DB_RELATION DB_SID DB_UNAME DB_PASSWD/) {
			if (! $param->{$_}) {
				ShowInfo("Please fill out all fields from the connection window!"); return undef
			}
		}
		
		### prepare DNS string for DBI's connection method
		my ($dns, $dbh);
		if ($param->{DB_TYPE} eq 'Oracle') { 
			$dns =  "dbi:Oracle:sid=$param->{DB_SID};host=$param->{DB_HOST};port=$param->{DB_PORT}";
		} 
		elsif ($param->{DB_TYPE} eq 'PostgreSQL') { 
			$dns =  "dbi:PgPP:dbname=$param->{DB_SID};host=$param->{DB_HOST};port=$param->{DB_PORT}";
		}
		elsif ($param->{DB_TYPE} eq 'MySQL') { 
			$dns =  "dbi:mysql:dbname=$param->{DB_SID};host=$param->{DB_HOST};port=$param->{DB_PORT}";
		}
		
		### connect to database and capture error messages
		eval { $dbh = DBI->connect( $dns, $param->{DB_UNAME},$param->{DB_PASSWD}, { AutoCommit => 1, RaiseError => 1}) };
		if ($@) {		
			ShowInfo($@), return undef;
		}
		
		### first statementhandle only to get column names
		my $sth = $dbh->prepare("select * from $param->{DB_RELATION}") or (ShowInfo($DBI::errstr), $dbh->disconnect, return undef);	
		$sth->execute or (ShowInfo($DBI::errstr), $dbh->disconnect, return undef);		
		my @names = @ { $sth->{NAME} };
		
		$sth->finish;
		
		### minimum of 6 columns are necessary
		if (scalar @names < 6) {  		
			ShowInfo("The relation $param->{DB_RELATION} does not complies with the requirements of at least 6 columns!");
			return;
		}
			
		my $ped_ref = $dbh->selectcol_arrayref("select distinct($names[0]) from $param->{DB_RELATION} order by $names[0]")  or (ShowInfo($DBI::errstr), $dbh->disconnect, return undef);   
		
		### No pedigrees found, something is bad
		if (!@$ped_ref ) {
			ShowInfo("The relation $param->{DB_RELATION} seems not to contain any data!");
			$dbh->disconnect;
			return;
		}
				
		my $d2 = $mw->DialogBox(-title => 'Choose Pedigrees',-buttons => ['Ok', 'Cancel']);
		
		### Chosing Pedigrees from the listbox
		my $f1 = $d2->Frame->grid(-row => 1, -column => 0, -sticky => 'w');
		my $lab1 = $f1->Label(-text => 'Pedigree Selection', -width => 20)->pack(-side => 'top', -anchor => 'w');
		my $lb = $f1->Scrolled('Listbox',
			-scrollbars => 'osoe', -selectmode => 'extended', -selectbackground => 	'red',
			-height => 14, -width => 25, -exportselection => 0,
		)->pack(-side => 'top', -fill => 'both', -expand => 1);
		$d2->gridColumnconfigure( 0, -pad => 10);
		foreach (@$ped_ref) { $lb->insert('end',$_) }
				
		my $answ = $d2->Show();		
		if ($answ eq 'Cancel') { $dbh->disconnect; return }
		
		### processing 
		else {											
			my @ped; 
			foreach ($lb->curselection) { push @ped, $lb->get($_) }
			return unless @ped;
			foreach (@ped) { $_ = "'$_'" }
			my $choose = join ',', @ped;
			my $aref = $dbh->selectall_arrayref("select * from $param->{DB_RELATION} where $names[0] in ($choose)") or (ShowInfo($DBI::errstr), $dbh->disconnect, return undef);
			$dbh->disconnect();
			return unless @$aref;
			
			%pedigree = ();
			%haplo = ();
			%map = ();
			undef $self->{HAPLO};
			
			### read the data in global Hash %pedigree
			foreach my $r (@$aref) {
				my $fam = shift @$r;
				push @{ $pedigree{$fam} }, [ @$r ];				
			}
			
			$param->{PEDIGREE_PATH} = "$param->{DB_TYPE} $param->{DB_SID}\@$param->{DB_HOST}";
			$param->{PEDIGREE_FORMAT} = 'PRAEMAKEPED_PLUS';
			undef $param->{HAPLO_PATH};
			
			### Updating Main Window menu
			my $fileref = $menubar->entrycget('View', -menu);
			my $drawref = $fileref->entrycget('Draw Pedigree ...', -menu);
			$drawref->delete(0,'end');
			for my $fam (nsort keys %pedigree) { $drawref->add('command', -label => $fam, -command => sub {DoIt($fam)} ) }
			DoIt(nsort keys %pedigree);
			$d->destroy();	
		}			  		                                                                                                                                               
	})->grid( -row => 0, -column => 0, -sticky => 'w');
	
	### Cancel bottom
	$fb->Button(-text => 'Cancel', -width => 10, -command => sub {
		$d->destroy();
	})->grid( -row => 0, -column => 1, -sticky => 'w');
	
	### Save Default bottom
	$fb->Button(-text => 'Save Default', -width => 10, -command => sub {
		open (FH, ">", "hp_dbi_default") or (ShowInfo("Unable to save current setting as default!\n$!"));
			foreach ( qw/ DB_TYPE DB_HOST DB_PORT DB_RELATION DB_SID DB_UNAME /) {
				print FH "$_=$param->{$_}\n" if $param->{$_}
			}
		close FH;
	})->grid( -row => 0, -column => 2, -sticky => 'w');
	
	### Load Default Bottom
	$fb->Button(-text => 'Load Default', -width => 10, -command => sub {		
		open (FH, "<", "hp_dbi_default") or (ShowInfo("Unable to load the default file:hp_dbi_default\n$!"));
			while (<FH>) {
				chomp;
				s/ //g;
				next unless $_;
				@_ = split "=",$_;
				if (@_ && $T{$_[0]}) {
					$param->{$_[0]} = $_[1];					
				}				
			}
		close FH;
	})->grid( -row => 0, -column => 3, -sticky => 'w');
		
	
	### BrowseEntry widget for Database Type
	$f1->Label(-text => 'Database type',-width => 13,-justify => 'left',-anchor => 'w',)->pack(-side => 'left');
	$f1->BrowseEntry(
		-textvariable => \$param->{DB_TYPE}, -choices => [ 'Oracle', 'PostgreSQL', 'MySQL' ],
		-bg => 'white',-disabledbackground => 'white',
		-width => 25, -state => 'readonly', -browsecmd => sub {
			if ($param->{DB_TYPE} eq 'Oracle') 				{ $param->{DB_PORT} = 1521 }
			elsif ($param->{DB_TYPE} eq 'PostgreSQL')	{ $param->{DB_PORT} = 5432 }
			elsif ($param->{DB_TYPE} eq 'MySQL')			{ $param->{DB_PORT} = 3306 }
		}
	)->pack(-padx => 11, -side => 'left');
		
	
	$f2->Label(-text => 'Hostname',-width => 15,-justify => 'left',-anchor => 'w',)->pack(-side => 'left');
	my $entry2 = $f2->Entry(-textvariable => \$param->{DB_HOST},-width => 25)->pack( -side => 'left');
		
	$f3->Label(-text => 'Port',-width => 15,-justify => 'left',-anchor => 'w')->pack(-side => 'left');
	$f3->Entry(-textvariable => \$param->{DB_PORT},-width => 25)->pack( -side => 'left');
	
	$f4->Label(-text => 'Table name',-width => 15,-justify => 'left',-anchor => 'w')->pack(-side => 'left');
	$f4->Entry(-textvariable => \$param->{DB_RELATION},-width => 25)->pack( -side => 'left');
	
	$f5->Label(-text => 'DBname(SID)',-width => 15,-justify => 'left',-anchor => 'w')->pack(-side => 'left');
	$f5->Entry(-textvariable => \$param->{DB_SID},-width => 25)->pack( -side => 'left');
		
	$f6->Label(-text => 'Username',-width => 15,-justify => 'left',-anchor => 'w')->pack(-side => 'left');
	$f6->Entry(-textvariable => \$param->{DB_UNAME},-width => 25)->pack( -side => 'left');
	
	$f7->Label(-text => 'Password',-width => 15,-justify => 'left',-anchor => 'w')->pack(-side => 'left');
	$f7->Entry(-textvariable => \$param->{DB_PASSWD}, -show => '*',-width => 25 )->pack( -side => 'left');
				
	$d->withdraw();
	$d->Popup();	
	$d->idletasks;
	$d->iconimage($d->Photo(-format =>'gif',-data => GetIconData()));
}


#==================
sub ImportPedfile {
#==================	
	my ($form, $f) = @_;
	if (!$f) { $f = $mw->getOpenFile() or return }
	ReadPed( -file => $f, -format => $form );
	### Updating Main Window menu
	my $fileref = $menubar->entrycget('View', -menu);
	my $drawref = $fileref->entrycget('Draw Pedigree ...', -menu);
	$drawref->delete(0,'end');
	for my $fam (nsort keys %pedigree) { $drawref->add('command', -label => $fam, -command => sub {DoIt($fam)} ) }
	DoIt(nsort keys %pedigree);
	if ($opt) { $opt->destroy() if Exists($opt); undef $opt }
}

#====================
sub ImportHaplofile {
#====================	
	return unless $self->{FAMILY};
	my ($format, $f) = @_;
	if (!$f) { $f = $mw->getOpenFile() or return }
	ReadHaplo( -file => $f, -format => $format ) or return;
	DuplicateHaplotypes();
	my $fam = $self->{FAMILY};
	if ($fam && $haplo{$fam}) {
		$self->{HAPLO} = $haplo{$fam};
		$self->{HAPLO}{MAP} = \%map;
		$self->{HAPLO_PATH} = $param->{HAPLO_PATH};
		ShuffleFounderColors();
		ProcessHaplotypes();
		RedrawPed();
		AdjustView();
		if ($opt) { $opt->destroy() if Exists($opt); undef $opt }
	}
}

#==================
sub ImportMapfile {
#==================	
	return unless $self->{FAMILY};
	my $f = $mw->getOpenFile() or return;
	ReadMap( -file => $f, -format => shift ) or return;
	$self->{HAPLO}{MAP} = \%map;
	RedrawPed();
	AdjustView();
	if ($opt) { $opt->destroy() if Exists($opt); undef $opt }
}

#=========
sub Zoom {
#=========	
	shift @_ if Exists ($_[0]);
	my ($ori, $flag, $x, $y) = @_;
	if ($x && $y) { ($param->{X},$param->{Y}) = ($x, $y) }	
	($x, $y) = ($canvas->canvasx($param->{X}), $canvas->canvasy($param->{Y}));
	
	if ($ori == 1 ) {	for ($self->{ZOOM}, $x, $y) { $_*=1.5 }}
	else {  for ($self->{ZOOM}, $x, $y) { $_/=1.5 } }	
	
	RedrawPed();
	
	if ($flag) { AdjustView(-fit => 'to_button', -x => $x, -y => $y) }
	else { AdjustView() }
}

#=======================
sub RedrawHaploShuffle {
#=======================	
	ShuffleColors();
	RedrawPed();
}

#==============
sub ShowAbout {
#==============	
	$mw->messageBox(
		-title => 'About HaploPainter',
		-message =>
		"Version: $param->{VERSION} \n" .
		"Last change: $param->{LAST_CHANGE}\n" .
		"Author: Holger Thiele\n" .
		"Contact: hthiele\@users.sourceforge.net\n\n" .
		'http://haplopainter.sourceforge.net',
		-type => 'OK', -icon => 'info'
	)		
}

#=================
sub OptionsPrint {
#=================	
	my $d = $mw->DialogBox(-title => 'Print Options',-buttons => ['Ok']);
	my $f = $d->Frame(-relief => 'groove', -borderwidth => 2)->pack( -padx => 5, -pady => 5, -expand => 1, -fill => 'both');

	my $be1 = $f->LabEntry(
		-label => 'Title: ', -labelPack => [ -side => 'left', -anchor => 'w' ],
		-textvariable => \$self->{TITLE}, -width => 50,
	)->grid(-row => 0, -column => 0, -columnspan => 2, -sticky => 'w');

	my $be2 = $f->BrowseEntry(
		-label => 'Font size: ',-variable => \$self->{FONT_HEAD}{SIZE}, -command => sub { },
		-width => 10,-choices =>	[ 5 .. 20, 22, 24, 26, 28, 36, 48, 72 ],
	)->grid(-row => 0, -column => 4, -columnspan => 3, -sticky => 'w');

	my $be3 = $f->BrowseEntry(
		-label => 'Orientation: ',-variable => \$param->{ORIENTATION},
		-width => 15,-choices =>	[ 'Landscape', 'Portrait' ],
	)->grid(-row => 1, -column => 1, -columnspan => 3, -sticky => 'w');

	my $be4 = $f->BrowseEntry(
		-label => 'Paper Size: ',-variable => \$param->{PAPER},
		-width => 15,-choices =>	[ nsort keys %{$param->{PAPER_SIZE}} ],
	)->grid(-row => 2, -column => 1, -columnspan => 3, -sticky => 'w');

	$f->Checkbutton( -text => "Show Head", -variable => \$param->{SHOW_HEAD},
	)->grid( -row => 1, -column => 4, -sticky => 'e');
	$f->Checkbutton( -text => "Show Date", -variable => \$param->{SHOW_DATE},
	)->grid( -row => 2, -column => 4, -sticky => 'e');
	
	$f->Checkbutton( -text => "Show pedigree file path", -variable => \$param->{SHOW_PEDFILE},
	)->grid( -row => 3, -column => 2, -sticky => 'w');
	$f->Checkbutton( -text => "Show haplotype file path", -variable => \$param->{SHOW_HAPLOFILE},
	)->grid( -row => 4, -column => 2, -sticky => 'w');
	
	$f->Radiobutton( -value => 0, -text => "No loop break", -variable => \$param->{LOOP_BREAK_STATUS},
	)->grid( -row => 3, -column => 1, -sticky => 'w');
	$f->Radiobutton( -value => 1, -text => "Loop break by chance", -variable => \$param->{LOOP_BREAK_STATUS},
	)->grid( -row => 4, -column => 1, -sticky => 'w');
	$f->Radiobutton( -value => 2, -text => "Break all loops", -variable => \$param->{LOOP_BREAK_STATUS},
	)->grid( -row => 5, -column => 1, -sticky => 'w');
	

	foreach my $s (
		[ 'BORDER_UP',		'Margin up',         1,0,   10,  300,    5  ],
		[ 'BORDER_DOWN',	'Margin down',       2,0,   10,  300,    5  ],
		[ 'BORDER_LEFT',	'Margin left',       3,0,   10,  300,    5  ],
		[ 'BORDER_RIGHT',	'Margin right',      4,0,   10,  300,    5  ],
		[ 'RESOLUTION',     'Resolution [dpi]',  5,0,   72,  600,    4  ],
	) {
		$f->Scale(
			-label  => @$s[1], -variable => \$param->{@$s[0]},
			-from   => @$s[4], -to => @$s[5],-orient => 'horizontal',
			-length => 150, -width => 12, -resolution => @$s[6],-command => sub {
			}
		)->grid( -row => @$s[2], -column => @$s[3], -sticky => 'w');
	}

	$d->Show();
	DoIt($self->{FAMILY})
}





# Configuratuion menu
#==================
sub Configuration {
#==================	
	### make copy of self for restoring data when cancel - action
	my $freeze = freeze($self);
		
	$opt = $mw->Toplevel(-title => 'Configuration');						
	$opt->withdraw();
	
	my $f1 = $opt->Frame(-relief => 'groove', -borderwidth => 2)->pack( -side => 'top', -padx => 5, -pady => 5, -expand => 1, -fill => 'both');
	my $f2 = $opt->Frame()->pack( -side => 'top', -padx => 5, -pady => 5,  -fill => 'x');
 	
	### Buttons on bottom
	$f2->Button(-text => 'Ok', -width => 10, -command => sub {
		BuildMatrix(); my $cc = 0;	until (AlignMatrix()) { $cc++ ; last if $cc > 120 }	
		RedrawPed() ;
		AdjustView(); 
		Default('update');
		$opt->destroy;
	})->grid( -row => 0, -column => 0, -sticky => 'w');
	
	$f2->Button(-text => 'Cancel', -width => 10, -command => sub {
		$self = thaw($freeze) if $self; 
		RedrawPed();
		AdjustView();
		$opt->destroy;
	})->grid( -row => 0, -column => 1, -sticky => 'w');
				
		
	$f2->Button(-text => 'Preview', -width => 10, -command => sub {
		BuildMatrix(); my $cc = 0;	until (AlignMatrix()) { $cc++ ; last if $cc > 120 }		
		RedrawPed();
		AdjustView();
	})->grid( -row => 0, -column => 3, -sticky => 'w');

	### Notebook
	my $n = $f1->NoteBook(
		-background => '#28D0F0'
	)->pack(-expand => 1, -fill => 'both');

	my $p1 = $n->add( 'page1' , -label => 'Hap Style');
	my $p2 = $n->add( 'page2' , -label => 'Hap Show');
	my $p3 = $n->add( 'page3' , -label => 'Font & Color');
	my $p4 = $n->add( 'page4' , -label => 'Hap Region');
	my $p5 = $n->add( 'page5' , -label => 'Line Style');
	my $p6 = $n->add( 'page6' , -label => 'Line Color');
	my $p7 = $n->add( 'page7' , -label => 'Case Info');
	

	###############################################################################
	### page1
	### place Scale Widgets
	foreach my $s (
		[ 'HAPLO_WIDTH'        , 'Bar width',                     0,0,   1, 50,    1  ],
		[ 'HAPLO_WIDTH_NI'     , 'Bar width uninformative',       1,0,   1, 50,    1  ],
		[ 'HAPLO_SPACE'        , 'Space between bars',            2,0,   1, 50,    1  ],
		[ 'HAPLO_LW'	       , 'Line width',                    3,0, 0.1, 10,  0.1  ],
		[ 'LEGEND_SHIFT_LEFT' , 'Legend distance left',           4,0,  20,500,    5  ],
		
		[ 'HAPLO_TEXT_LW'      , 'Allele distance',               0,1,   0,  5,  0.1  ],						
		[ 'MARKER_POS_SHIFT'   , 'Marker <-> position distance',  1,1,-500,500,    5  ],
		[ 'ALLELES_SHIFT'      , 'Allele position distance',      2,1,   0,100,    1  ],
		[ 'BBOX_WIDTH'         , 'Width of boundig boxes',        3,1,  10,100,    1  ],
		[ 'LEGEND_SHIFT_RIGHT' , 'Legend distance right',         4,1,  20,500,    5  ],
	) {
		$p1->Scale(
			-label  => @$s[1], -variable => \$self->{@$s[0]},
			-from   => @$s[4], -to => @$s[5],-orient => 'horizontal',
			-length => 150, -width => 12, -resolution => @$s[6],
		)->grid( -row => @$s[2], -column => @$s[3], -sticky => 'ns');
		$p1->gridColumnconfigure( @$s[2], -pad => 50);
	}

	###############################################################################
	### page2
	### place Checkbuttons
	foreach my $s (
		[ 'SHOW_HAPLO_TEXT'	   , 'Show alleles',                            0,0  ],
		[ 'SHOW_HAPLO_BAR'	   , 'Show bars',                               1,0  ],
		[ 'SHOW_POSITION'      , 'Show marker positions',                   2,0  ],
		[ 'SHOW_MARKER'        , 'Show marker names',                       3,0  ],
		[ 'SHOW_HAPLO_BBOX'    , 'Show haplotypes bounding box',            4,0  ],
		[ 'SHOW_QUEST'         , 'Show question mark',                      5,0  ],
		[ 'ALIGN_LEGEND',      , 'Justify map legend',                      6,0  ],
		[ 'SHOW_COLORED_TEXT', , 'Show alleles like bar colours',           7,0  ],
		[ 'FILL_HAPLO'		   , 'Fill out bars',                           0,1  ],
		[ 'SHOW_HAPLO_NI_0'    , 'Show completly lost Haplotypes',          1,1  ],
		[ 'SHOW_HAPLO_NI_1'    , 'Show other lost genotypes',               2,1  ],
		[ 'SHOW_HAPLO_NI_2'    , 'Show user defined non-informative',       3,1  ],
		[ 'SHOW_HAPLO_NI_3'    , 'Show other non-informative',              4,1  ],
		[ 'HAPLO_SEP_BL'       , 'Draw each allele as separate bar',         5,1  ],
		[ 'SHOW_LEGEND_LEFT'   , 'Show legend left',                        6,1  ],
		[ 'SHOW_LEGEND_RIGHT'  , 'Show legend right',                       7,1  ],
	) {
		$p2->Checkbutton( -text => @$s[1], -variable => \$self->{@$s[0]},
		)->grid( -row => @$s[2], -column => @$s[3], -sticky => 'w');
		$p2->gridColumnconfigure( @$s[2], -pad => 30);
	}

	###############################################################################
	### page3
	### Fonts + Colors
	my $hap_f = $p3->Frame->grid(-row => 0, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $hap_l = $hap_f->Label(-width => 3, -bg => $self->{FONT_HAPLO}{COLOR})->pack(-side => 'left', -padx => 10);
	my $hap_b; $hap_b = $hap_f->Button( -text => 'Haplotype Font', -width => 20, -command => sub {
		ChooseFont('FONT_HAPLO', $hap_l);
	})->pack(-side => 'left');
	my $inf_f = $p3->Frame->grid(-row => 1, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $inf_l = $inf_f->Label(-width => 3, -bg => $self->{FONT1}{COLOR})->pack(-side => 'left', -padx => 10);
	my $inf_b; $inf_b = $inf_f->Button( -text => 'Symbol information Font', -width => 20,-command => sub {
		ChooseFont('FONT1', $inf_l)
	})->pack(-side => 'left');
	my $head_f = $p3->Frame->grid(-row => 2, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $head_l = $head_f->Label(-width => 3, -bg => $self->{FONT_HEAD}{COLOR})->pack(-side => 'left', -padx => 10);
	my $head_b; $head_b = $head_f->Button( -text => 'Title Font', -width => 20, -command => sub {
		ChooseFont('FONT_HEAD', $head_l)
	})->pack(-side => 'left');


	### Farbe fuer HAPLO_UNKNOWN
	my $fc2 = $p3->Frame->grid(-row => 3, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $lb2 = $fc2->Label(-width => 3, -bg => $self->{HAPLO_UNKNOWN_COLOR})->pack(-side => 'left', -padx => 10);
	my $ub; $ub = $fc2->Button(
		-text => 'Phase unknown color',
		-width => 20, -height => 1,-command => sub {
			my $col = $mw->chooseColor() or return;
			$self->{HAPLO_UNKNOWN_COLOR} = $col;
			$lb2->configure(-bg => $col)
	})->pack(-side => 'left');

	### Farbe f�r ALLE HAPLOTYPEN
	my $fc5 = $p3->Frame->grid(-row => 4, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $lb5 = $fc5->Label(-width => 3, -bg => $self->{HAPLO_UNKNOWN_COLOR})->pack(-side => 'left', -padx => 10);
	$fc5->Button(-text => 'Color of all haplotypes ',-width => 20, -height => 1,-command => sub {
		my $col_new = $mw->chooseColor() or return;
		foreach my $p (keys %{$self->{FOUNDER}}) {
			next unless $self->{HAPLO}{PID}{$p};
			foreach my $mp ( 'M', 'P' ) {
				next if $self->{HAPLO}{PID}{$p}{$mp}{SAVE} ;
				foreach (@ { $self->{HAPLO}{PID}{$p}{$mp}{BAR} }) {@$_[1] = $col_new }
			}
		}
		$lb5->configure(-bg => $col_new);
		$opt->focusForce;
		ProcessHaplotypes();
	})->pack(-side => 'left');


	my $fc3 = $p3->Frame->grid(-row => 1, -column => 2, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $lb3 = $fc3->Label(-width => 3)->pack(-side => 'left', -padx => 10);
	my ($pb, $pid);
	$pb = $fc3->Button(
		-text => 'Color of paternal Haplotype',
		-width => 25, -height => 1,-command => sub {					
			if ($pid && $self->{HAPLO}{PID}{$pid} && $self->{HAPLO}{PID}{$pid}{P}{BAR}[0]) {
				my $col_new = $n->chooseColor() or return;
				my $col_old = $self->{HAPLO}{PID}{$pid}{P}{BAR}[0][1] or return;
				ChangeColor($col_old, $col_new);
				$lb3->configure(-bg => $col_new);
				$opt->focusForce;	
			}
	})->pack(-side => 'left');

	my $fc4 = $p3->Frame->grid(-row => 2, -column => 2, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $lb4 = $fc4->Label(-width => 3)->pack(-side => 'left', -padx => 10);
	my $mb; $mb = $fc4->Button(
		-text => 'Color of maternal Haplotype',
		-width => 25, -height => 1,-command => sub {
			if ($pid && $self->{HAPLO}{PID}{$pid} && $self->{HAPLO}{PID}{$pid}{M}{BAR}[0]) {
				my $col_new = $n->chooseColor() or return;
				my $col_old = $self->{HAPLO}{PID}{$pid}{M}{BAR}[0][1] or return;		
				ChangeColor($col_old, $col_new);
				$lb4->configure(-bg => $col_new);
				$opt->focusForce;
			}
	})->pack(-side => 'left');


	my $cbs1 = $p3->Checkbutton( -text => 'Anchor paternal haplotype',
	)->grid( -row => 3, -column => 2, -sticky => 'w');
	my $cbs2 = $p3->Checkbutton( -text => 'Anchor maternal haplotype',
	)->grid( -row => 4, -column => 2, -sticky => 'w');

	my $cbs3 = $p3->Checkbutton( -text => 'Hide paternal haplotype',
	)->grid( -row => 5, -column => 2, -sticky => 'w');
	my $cbs4 = $p3->Checkbutton( -text => 'Hide maternal haplotype',
	)->grid( -row => 6, -column => 2, -sticky => 'w');


	### personenbezogene Einstellungen
	@_ = nsort keys %{$self->{FOUNDER}};
	my $be5 = $p3->BrowseEntry(
		-label => '        Founder:', -variable => \$pid,
		-choices => [ @_  ], -width => 15,	-state => 'readonly',
		-browsecmd => sub {
			foreach (@{$self->{HAPLO}{PID}{$pid}{P}{BAR}}) {
				if (@$_[1] ne $self->{HAPLO_UNKNOWN_COLOR}) { $lb3->configure(-bg => @$_[1]); last }
			}
			foreach (@{$self->{HAPLO}{PID}{$pid}{M}{BAR}}) {
				if (@$_[1] ne $self->{HAPLO_UNKNOWN_COLOR}) { $lb4->configure(-bg => @$_[1]); last }
			}
			if ($self->{HAPLO}{PID}{$pid}{P}{SAVE}) {$_ = 'disabled'}
			else { $_ = 'normal' } $pb->configure(-state => $_);
			if ($self->{HAPLO}{PID}{$pid}{M}{SAVE}) {$_ = 'disabled'}
			else { $_ = 'normal' } $mb->configure(-state => $_);
			$cbs1->configure(
				-variable =>  \$self->{HAPLO}{PID}{$pid}{P}{SAVE},
				-command => sub {
					if ($self->{HAPLO}{PID}{$pid}{P}{SAVE}) {
						$pb->configure(-state => 'disabled')
					} else { $pb->configure(-state => 'normal')	}
			});
			$cbs2->configure(
				-variable =>  \$self->{HAPLO}{PID}{$pid}{M}{SAVE},
				-command => sub {
					if ($self->{HAPLO}{PID}{$pid}{M}{SAVE}) {
						$mb->configure(-state => 'disabled')
					} else { $mb->configure(-state => 'normal')	}
			});
			$cbs3->configure(-variable =>  \$self->{HAPLO}{PID}{$pid}{P}{HIDE});
			$cbs4->configure(-variable =>  \$self->{HAPLO}{PID}{$pid}{M}{HIDE});
			$opt->focusForce;

	})->grid(-row => 0, -column => 2, -sticky => 'w');
	for ( 0..8 ) { $p3->gridRowconfigure( $_, -pad => 10) }
	$p3->gridColumnconfigure( 0, -pad => 40) ;

	###############################################################################
	### page4
	### Listbox Markerauswahl +  Bounding Box
	### Markerauswahl
	my $f5 = $p4->Frame->grid(-row => 1, -column => 0, -sticky => 'w');
	my $lab5 = $f5->Label(-text => 'Marker Selection', -width => 20)->pack(-side => 'top', -anchor => 'w');
	my $lb = $f5->Scrolled('Listbox',
		-scrollbars => 'osoe', -selectmode => 'extended', -selectbackground => 	'red',
		-height => 14, -width => 25, -exportselection => 0,
	)->pack(-side => 'top', -fill => 'both', -expand => 1);
	$p4->gridColumnconfigure( 0, -pad => 10);

	if ($self->{HAPLO}{MAP}{MARKER}) {
		@_ = @{$self->{HAPLO}{MAP}{MARKER}};
		for (my $i = 0; $i < scalar @_; $i++) {
			my $j = ''; unless ( defined $self->{HAPLO}{MAP}{POS}[$i] ) {
				$j = sprintf ("%03.0f - ", $i+1);
			} else {
				$j = sprintf ("%6.2f - ", $self->{HAPLO}{MAP}{POS}[$i]);
			}

			$lb->insert('end', "$j$_[$i]");
			$lb->selectionSet($i) if $self->{HAPLO}{DRAW}[$i];
		}
	}

	$lb->bind('<ButtonRelease-1>' => sub {
		if ($self->{HAPLO}{MAP}{MARKER}) {
			my %h; foreach ($lb->curselection()) { $h{$_} = 1 }
			for (my $i = 0; $i < scalar @{$self->{HAPLO}{MAP}{MARKER}}; $i++) {
				if ($h{$i}) { $self->{HAPLO}{DRAW}[$i] = 1 }
				else { $self->{HAPLO}{DRAW}[$i] = 0 }
			}
		}
	});

	$opt->bind('<Control-Key-a>' => sub {
		if ($self->{HAPLO}{MAP}{MARKER}) {
			@_ = @{$self->{HAPLO}{MAP}{MARKER}};
			if (@_) {
				for (my $i = 0; $i < scalar @_; $i++) {
					$lb->selectionSet($i) ;
					$self->{HAPLO}{DRAW}[$i] = 1;
				}
			}
		}
	});

	### Boundig Box
	@_ = nsort keys %{$self->{PID}};
	my ($person, $lb6);
	my $be6 = $p4->BrowseEntry(
		-label => 'Sample', -variable => \$person,
		-choices => [ @_  ], -width => 15,	-state => 'readonly',
		-browsecmd => sub {
			if ($self->{HAPLO}{PID}{$person}{P}{TEXT}) {
				my $h = $self->{HAPLO}{PID}{$person};
				@_ = @{$self->{HAPLO}{MAP}{MARKER}};
				$lb6->delete(0,'end');
				for (my $i = 0; $i < scalar @_; $i++) {
					my $j = ''; if ( @{ $self->{HAPLO}{MAP}{POS} } ) {
						$j = sprintf ("%6.2f - ", $self->{HAPLO}{MAP}{POS}[$i]);
					}
					my $alstr = "($h->{P}{TEXT}[$i]\\$h->{M}{TEXT}[$i])";
					$lb6->insert('end', "$j$_[$i] $alstr");
					$lb6->selectionSet($i) if $h->{BOX}[$i];
				}
			}
	})->grid(-row => 0, -column => 1, -sticky => 's');
	$p4->gridRowconfigure( 1, -pad => 10);

	my $f6 = $p4->Frame->grid(-row => 1, -column => 1, -rowspan => 7, -sticky => 'w');
	my $lab6 = $f6->Label(-text => 'Boundig Box Selection', -width => 20)->pack(-side => 'top', -anchor => 'w');
	$lb6 = $f6->Scrolled('Listbox',
		-scrollbars => 'osoe', -selectmode => 'extended', -selectbackground => 	'red',
		-height => 14, -width => 25, -exportselection => 0,
	)->pack(-side => 'top', -fill => 'both', -expand => 1);
	$p4->gridColumnconfigure( 1, -pad => 10);

	$lb6->bind('<ButtonRelease-1>' => sub {
		if ($self->{HAPLO}{MAP}{MARKER}) {
			my %h; foreach ($lb6->curselection()) { $h{$_} = 1 }
			for (my $i = 0; $i < scalar @{$self->{HAPLO}{MAP}{MARKER}}; $i++) {
				if ($h{$i}) { $self->{HAPLO}{PID}{$person}{BOX}[$i] = 1 }
				else { $self->{HAPLO}{PID}{$person}{BOX}[$i] = 0 }
			}
		}
	});

	###############################################################################
	### page5
	### Lines Option Schieberegler
	foreach my $s (
		[ 'CROSS_FAKTOR1',	'Cross factor',             0,0,   0.1,  5,  0.1  ],
		[ 'ALIVE_SPACE',	'Dead line length',         1,0,     1, 20,    1  ], 
		[ 'GITTER_X',		'Grid X space',             2,0,     5, 50,    1  ],
		[ 'GITTER_Y',		'Grid Y space',             3,0,     5, 50,    1  ],
		[ 'CONSANG_DIST',	'Consanguine line distance',4,0,     1, 10,    1  ],
		[ 'SYMBOL_SIZE',	'Symbol size',              0,1,     5, 50,    1  ],
		[ 'LINE_WIDTH',		'Symbol outer line width',  1,1,   0.1,  5,  0.1  ],
		[ 'X_SPACE',		'Inter symbol distance',    2,1,     1, 20,    1  ],			
		[ 'Y_SPACE_DEFAULT','Inter generation distance',3,1,     3, 50,    1  ],
		[ 'Y_SPACE_EXTRA',  'Haplo extra space',        4,1,    -5,  5,  0.1  ],
	) {
		$p5->Scale(
			-label  => @$s[1], -variable => \$self->{@$s[0]},
			-from   => @$s[4], -to => @$s[5],-orient => 'horizontal',
			-length => 150, -width => 12, -resolution => @$s[6],-command => sub {
				 #$flag = 1 if @$s[0] eq 'X_SPACE'					
			}
		)->grid( -row => @$s[2], -column => @$s[3], -sticky => 'w');
		$p5->gridColumnconfigure( @$s[2], -pad => 50);
	}

	###############################################################################
	### page6
	### Lines Option Farben
	my ($cb2,$cb3,$cb4,$cb5, $cb6);
	my $fl2 = $p6->Frame->grid(-row => 0, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $llb2 = $fl2->Label(-width => 3, -bg => $self->{AFF_COLOR}{1})->pack(-side => 'left', -padx => 10);
	$cb2 = $fl2->Button(
		-text => 'Not affected color',
		-width => 20, -height => 1,-command => sub {
			my $NewCol = $mw->chooseColor() or return;
			$self->{AFF_COLOR}{1} = $NewCol;
			$llb2->configure(-bg => $NewCol);
			$opt->focusForce;
	})->pack(-side => 'left');

	my $fl3 = $p6->Frame->grid(-row => 1, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $llb3 = $fl3->Label(-width => 3, -bg => $self->{AFF_COLOR}{2})->pack(-side => 'left', -padx => 10);
	$cb3 = $fl3->Button(
		-text => 'Affected color',
		-width => 20, -height => 1,-command => sub {
			my $NewCol = $mw->chooseColor() or return;
			$self->{AFF_COLOR}{2} = $NewCol;
			$llb3->configure(-bg => $NewCol);
			$opt->focusForce;
	})->pack(-side => 'left');

	my $fl4 = $p6->Frame->grid(-row => 2, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $llb4 = $fl4->Label(-width => 3, -bg => $self->{AFF_COLOR}{0})->pack(-side => 'left', -padx => 10);
	$cb4 = $fl4->Button(
		-text => 'Unknown status color',
		-width => 20, -height => 1,-command => sub {
			my $NewCol = $mw->chooseColor() or return;
			$self->{AFF_COLOR}{0} = $NewCol;
			$llb4->configure(-bg => $NewCol);
			$opt->focusForce;
	})->pack(-side => 'left');

	my $fl5 = $p6->Frame->grid(-row => 3, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $llb5 = $fl5->Label(-width => 3, -bg => $self->{LINE_COLOR})->pack(-side => 'left', -padx => 10);
	$cb5 = $fl5->Button(
		-text => 'Line color',
		-width => 20, -height => 1,-command => sub {
			my $NewCol = $mw->chooseColor() or return;
			$self->{LINE_COLOR} = $NewCol;
			$llb5->configure(-bg => $NewCol);
			$opt->focusForce;
	})->pack(-side => 'left');

	my $fl6 = $p6->Frame->grid(-row => 4, -column => 0, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $llb6 = $fl6->Label(-width => 3, -bg => $self->{BACKGROUND})->pack(-side => 'left', -padx => 10);
	$cb6 = $fl6->Button(
		-text => 'Background color',
		-width => 20, -height => 1,-command => sub {
			my $NewCol = $mw->chooseColor() or return;
			$self->{BACKGROUND} = $NewCol;
			$llb6->configure(-bg => $NewCol);
			$canvas->configure(-bg => $self->{BACKGROUND});
			$opt->focusForce;
	})->pack(-side => 'left');

	for ( 0..4 ) { $p6->gridRowconfigure( $_, -pad => 10) }

	###############################################################################
	### page7
	### case info
	foreach my $col ( 1 .. 4 ) {			
		$p7->BrowseEntry(
			-label => 'Case  ' . $col,
			-labelPack => [ -side => 'left', -anchor => 'w' ],
			-command => sub { },							
			-variable => \$self->{CASE_INFO}{COL_TO_NAME}{$col},
			-choices => [  nsort keys  % { $self->{CASE_INFO}{COL_NAMES} } ]
		)->grid(-row => $col, -column => 0,  -sticky => 'w');
		$p7->Checkbutton( -text => 'Show',
			-variable => \$self->{CASE_INFO_SHOW}{$col}
		)->grid( -row => $col, -column => 1, -sticky => 's');
	}			
	
	$opt->Popup();	
	$opt->idletasks;
	$opt->iconimage($opt->Photo(-format =>'gif',-data => GetIconData()));
	
	### to prevent manipulation from main wain window prior updated defaults 
	$opt->grab;
}    	


#===============
sub ChooseFont {
#===============	
	my ($k,$lab) = @_;
	my ($a, $c, $cb1);
	my $fo = $self->{$k};
	$opt->grabRelease;
	my $tl = $mw->Toplevel();
	$tl->title('Font');
	

	my $f1 = $tl->Frame(-relief => 'groove', -borderwidth => 2)->pack( -side => 'top', -padx => 5, -pady => 5, -expand => 1, -fill => 'both');
	my $f2 = $tl->Frame()->pack( -side => 'top', -padx => 5, -pady => 5,  -fill => 'x');
		
	
	### Font Familie
	my $fe1 = $f1->Frame->grid(-row => 0, -column => 1, -sticky => 'w');
	my $lab1 = $fe1->Label(-text => 'Font:', -width => 6)->pack(-side => 'left', -anchor => 'w');
	my $be1 = $fe1->BrowseEntry(
		-variable => \$fo->{FAMILY}, -state => 'readonly',
		-choices => [ nsort $mw->fontFamilies, 'Lucida' ],
		-command => sub { $cb1->configure(-font => [ $fo->{FAMILY}, 8, $fo->{WEIGHT}, $fo->{SLANT} ]) }
	)->pack(-side => 'left');

    ### Font Groesse
    my $fe2 = $f1->Frame->grid(-row => 1, -column => 1, -sticky => 'w');
    my $lab2 = $fe2->Label(-text => 'Size:', -width => 6)->pack(-side => 'left', -anchor => 'w');
    my $be2 = $fe2->BrowseEntry(
		-variable => \$fo->{SIZE}, -state => 'readonly',
		-choices => [ 5 .. 20, 22, 24, 26, 28, 36, 48, 72 ],
	)->pack(-side => 'left');

	### Font Weight
	my $fe3 = $f1->Frame->grid(-row => 2, -column => 1, -sticky => 'w');
    my $lab3 = $fe3->Label(-text => 'Weight:', -width => 6)->pack(-side => 'left', -anchor => 'w');
    my $be3 = $fe3->BrowseEntry(
		-variable => \$fo->{WEIGHT},-choices => [ 'bold', 'normal' ], -state => 'readonly',
		-command => sub { $cb1->configure(-font => [ $fo->{FAMILY}, 10, $fo->{WEIGHT}, $fo->{SLANT} ]) }
	)->pack(-side => 'left');

	### Font Style
	my $fe4 = $f1->Frame->grid(-row => 3, -column => 1, -sticky => 'w');
    my $lab4 = $fe4->Label(-text => 'Slant:', -width => 6)->pack(-side => 'left', -anchor => 'w');
    my $be4 = $fe4->BrowseEntry(
		-variable => \$fo->{SLANT},  -state => 'readonly',-choices => [ 'italic', 'roman',  ]	,
		-command => sub { $cb1->configure(-font => [ $fo->{FAMILY}, 10, $fo->{WEIGHT}, $fo->{SLANT} ]) }
	)->pack(-side => 'left');

	### Font Farbe
	my $fc1 = $f1->Frame->grid(-row => 5, -column => 1, -sticky => 'w');                                                                                                                                                                                 	### Font Farbe
	my $lb1 = $fc1->Label(-width => 3, -bg => $fo->{COLOR})->pack(-side => 'left', -padx => 10);
	$cb1 = $fc1->Button(
		-text => 'Choose Font Color',
		-font => [ $fo->{FAMILY}, 10, $fo->{WEIGHT}, $fo->{SLANT} ],
		-width => 24, -height => 1,-command => sub {
			my $NewCol = $mw->chooseColor() or return;
			$fo->{COLOR} = $NewCol;
			$lb1->configure(-bg => $NewCol);
			$lab->configure(-bg => $fo->{COLOR}) if $lab;
			$tl->focusForce;
	})->pack();

	$f1->gridRowconfigure( 5, -pad => 30);

	$f2->Button(-text => 'Ok', -width => 10, -command => sub {
		$tl->destroy(); 
		$opt->focusForce;
		$opt->grab;
	})->grid( -row => 0, -column => 0, -sticky => 'w');
	
	$tl->withdraw();
	$tl->Popup();	
	$tl->idletasks;
 	$tl->iconimage($opt->Photo(-format =>'gif',-data => GetIconData()));
	
}

#================
sub ChangeColor {
#================	
	my ($col_old, $col_new) = @_;
	return unless $self->{HAPLO}{PID};
	foreach my $p (keys %{$self->{PID}}) {
		next unless $self->{HAPLO}{PID}{$p};
		foreach my $mp ( 'M', 'P' ) {
			foreach (@ { $self->{HAPLO}{PID}{$p}{$mp}{BAR} }) {
				@$_[1] = $col_new if $col_old eq @$_[1];
			}
		}
	}
}

# Postscript output
# Exact fitting the graphic still not optimal
#===========
sub Export {
#===========
	return unless $self->{FAMILY};
	my $graphic_format = shift || $param->{GRAPHIC_FORMAT};
		
	if (! $batch && ! $param->{EXPORT_PATH} && ! ($graphic_format eq 'Print') ) {
		my $suffix = $param->{GRAPHIC_FORMATS_OK}{$graphic_format} || BatchError('Bad graphic format !');
		$param->{EXPORT_PATH}=$mw->getSaveFile(-initialfile => "$self->{FAMILY}.$suffix" ) or return
	}
			                          		
	my $paper = $param->{PAPER};
	my $out;
	
	my ($x1, $y1, $x2, $y2) = $canvas->bbox('all');
	$x1 -= $param->{BORDER_LEFT};
	$x2 += $param->{BORDER_RIGHT};
	$y1 -= $param->{BORDER_UP};
	$y2 += $param->{BORDER_DOWN};

	my $xdiff = $x2-$x1;
	my $ydiff = $y2-$y1;

	my ($cx, $cy) = ($canvas->width,  $canvas->height);
	my ($pxm, $pym) = ( $param->{PAPER_SIZE}{$paper}{X}, $param->{PAPER_SIZE}{$paper}{Y} );
	my @scale;
	my ($startx, $starty);	
	my ($xdim, $ydim, $f);		
	
	if ( ($param->{ORIENTATION} eq 'Landscape') && (($graphic_format eq 'postscript')  || ($graphic_format eq 'Print'))) {
		if ( $xdiff/$ydiff > sqrt(2) ) {
			@scale = ( -pagewidth  => $pym .'m');
			$f = $xdiff/$mw->pixels($pym . 'm');
			$ydim = $ydiff/$f;
			$xdim = $xdiff/$f;
			$startx = ($mw->pixels($pxm . 'm')-$ydim)/3;
			$starty = 0;
		}
		else {
			@scale = ( -pageheight  => $pxm .'m' );
			$f = $ydiff/$mw->pixels($pxm . 'm');
			$xdim = $xdiff/$f;
			$starty = ($mw->pixels($pym . 'm')-$xdim)/3;
			$startx = 0;
		}
		$out = $canvas->postscript(
			-rotate => 1,
			-pageanchor => 'nw',
			-pagex  => $startx,
			-pagey  => $starty,
			-x => $x1 ,
			-y => $y1 ,
			-width  => $xdiff,
			-height => $ydiff,
			@scale
		)
	} else {
		### Image ist 'breit' -> Scaling X
		if ( $xdiff/$ydiff > sqrt(2) ) {
			@scale = ( -pagewidth  => $pxm .'m');
			$f = $xdiff/$mw->pixels($pxm . 'm');
			$ydim = $ydiff/$f;
			$starty = ($mw->pixels($pym . 'm')-$ydim)/2.66;    ### empirical value
			$startx = 0;
		} else {
			@scale = ( -pageheight  => $pym .'m' );
			$f = $ydiff/$mw->pixels($pym . 'm');
			$xdim = $xdiff/$f;
			$startx = ($mw->pixels($pxm . 'm')-$xdim)/2.66;
			$starty = 0;
		}
		
		if (! $batch && (($graphic_format eq 'postscript') || ($graphic_format eq 'Print'))) {
			
			$out = $canvas->postscript(
				-rotate => 0,
				-pageanchor => 'sw',
				-pagex  => $startx,
				-pagey  => $starty,
				-x => $x1,
				-y => $y1,
				-width  => $xdiff ,
				-height => $ydiff ,
				@scale
			);
			
		} else {
			$out = $canvas->postscript(			
				-rotate => 0,
				-pagewidth  => $xdiff,				
				-width  => $xdiff,
				-height => $ydiff,
				-pageanchor => 'sw',
				-pagex  => 0,
				-pagey  => 0,
				-x => $x1,
				-y => $y1,				
				@scale
			);
		}																		
	}
	
	if ($graphic_format eq 'postscript') {
		open (FH, ">", $param->{EXPORT_PATH}) or ShowInfo($!) and return;
		print FH $out;
		close FH;
		undef $param->{EXPORT_PATH};
	} 
	elsif ($graphic_format eq 'Print') { return $out }
		
	else {
		my $file = basename($param->{EXPORT_PATH});
		my $dir = dirname($param->{EXPORT_PATH});
		if ($dir ne '.') { chdir $dir }
		open (FH, ">", 'tmp.ps') or ShowInfo($!) and return;
		print FH $out;
		close FH;	
		
		$out =~ /BoundingBox: 0 0 (\d+) (\d+)/;			
					
		@_ = (
		$param->{PRINT_SUPPORT}{$^O} || 'gs',
		'-dSAFER', '-dBATCH', '-dNOPAUSE', '-dNOPROMT',  '-q',
		"-dDEVICEWIDTHPOINTS=$1",
		"-dDEVICEHEIGHTPOINTS=$2",
		"-sDEVICE=$graphic_format",
		"-dTextAlphaBits=$param->{TEXT_ALPHA_BITS}",		
		"-r$param->{RESOLUTION}",
		"-dBackgroundColor=16$self->{BACKGROUND}"
		);
		
		if (! $graphic_format eq 'pdfwrite') {
			push @_, "-dGraphicsAlphaBits=$param->{GRAPHICS_ALPHA_BITS}",
		}
				
		push @_, ("-sOutputFile=$file","tmp.ps");																															
						
		ShowInfo("Unable to covert graphics ! Did you forget to install 'Ghostscript' ?\n") if system (join ' ', @_) != 0;				
		unlink 'tmp.ps';
		undef $param->{EXPORT_PATH};
	}			
	1;
}


# Export all pedigrees as postscript
# new function since V.024b
#================
sub BatchExport {
#==============
	ShowInfo("Please select working directory and a basic file name without suffix.\nGraphic outputs will be extended by pedigree identifiers.\n\n");
	my $file = $mw->getSaveFile(-initialfile => 'pedigree') or return;	
	my $graphic_format=shift;
	my $suffix = $param->{GRAPHIC_FORMATS_OK}{$graphic_format};
	foreach (nsort keys %pedigree) {
		$param->{EXPORT_PATH} = File::Spec->catfile( dirname($file), basename($file) . '_' . $_ . '.' . $suffix);
		DoIt($_);
		$canvas->update;		
		Export($graphic_format);
	}	
}

# Print functions
# Windows -> postscript is directed to PrintFile 
# Linux   -> postscript is directed GtkLP, this could also work for other unix systems
# but need to be tested
#==========
sub Print {
#==========	
	
	unless ($param->{PRINT_SUPPORT}{$^O}) {
		ShowInfo("For this system there is no print support available !\nPlease contact the author.\n", 'warning'); return
	}
	
	my $ps = Export('Print') or return;
	
	open (FH, ">" , 'temp.ps') or die $!;
	print FH $ps;
	close FH;

	if ($^O eq 'MSWin32') {
		system ("gsprint -query temp.ps ") == 0 or ShowInfo("Unable to print ! Did you forget to install 'GSview' ?\n", 'warning');	
	}
	elsif ($^O eq 'linux') {
		system ("gtklp  temp.ps") == 0 or ShowInfo("Unable to print ! Did you forget to install 'GtkLP' ?\n", 'warning');
	}
		
	unlink 'temp.ps';
}



# Anpassung der Canvas Scrollregion und Zentrierung der View oder/und
# Pedigree an Fenstergroesse anpassen
#===============
sub AdjustView {
#===============	
	my %arg = @_;
	my $c = $canvas;
	my @bx;
	my $flag;
	return if $batch;
	
	if ($grid) {
		$grid = 0;
		$flag = 1;
		ShowGrid();
	}

	@bx = $c->bbox('all');
	unless (@bx) {
		if ($flag) { $grid = 1; ShowGrid() }
		return;
	}

	my @xv = $c->xview;
	my @yv = $c->yview;
	my @sc = $c->Subwidget('canvas')->cget(-scrollregion);

	### Groesse der Bounding Box
	my $xbd = $bx[2]-$bx[0];
	my $ybd = $bx[3]-$bx[1];
			
	### Groesse des aktuellen Fensters (als Anteil der Scrollregion)
	my $xvd = $xv[1]-$xv[0];
	my $yvd = $yv[1]-$yv[0];

	### Groesse der Scrollregion
	my $xsd = $sc[2]-$sc[0];
	my $ysd = $sc[3]-$sc[1];
	
	### sichtbares Fenster
	my $wx = $xsd*$xvd;
	my $wy = $ysd*$yvd;
	
	### Scrollpuffer
	my ($scrx, $scry) = ($xbd, $ybd);
	if ($scrx < (1.5*$wx)) { $scrx = 1.5*$wx }
	if ($scry < (1.5*$wy)) { $scry = 1.5*$wy }

	if (! $arg{-fit}) {
		$c->configure(-scrollregion => [ $bx[0]-$scrx, $bx[1]-$scry, $bx[2]+$scrx, $bx[3]+$scry ]);
		### Zentrierung der Scrollbalken
		$c->xviewMoveto(0.5-($xvd*0.5));
		$c->yviewMoveto(0.5-($yvd*0.5));
	}
	elsif ( $arg{-fit} eq 'center') {					
		if  ($xbd/$ybd > $wx/$wy) {$self->{ZOOM} *= $wx/$xbd*0.9} else { $self->{ZOOM} *= $wy/$ybd*0.9  }
		RedrawPed();
		my @bx = $canvas->bbox('all');
		$c->configure(-scrollregion => [ $bx[0]-$scrx, $bx[1]-$scry, $bx[2]+$scrx, $bx[3]+$scry ]);
		AdjustView();
	}

	elsif ($arg{-fit} eq 'to_button') {
		my $x = $arg{-x};
		my $y = $arg{-y};
		$c->configure(-scrollregion => [ $x-$scrx, $y-$scry, $x+$scrx, $y+$scry ]);
		### Zentrierung der Scrollbalken
		$c->xviewMoveto(0.5-($xvd*0.5));
		$c->yviewMoveto(0.5-($yvd*0.5));
	}
	if ($flag) { $grid = 1; ShowGrid() }
}

#==============
sub RedrawPed {
#==============	
	FillCanvas();
	SetLines();
	DrawLines();		
	DrawHaplo();
}

#=============
sub ShowGrid {
#=============	
	my $z = $self->{ZOOM};
	if ($grid) {
		$canvas->createGrid( 0,0, $self->{GITTER_X}*$z,$self->{GITTER_Y}*$z,-lines => 1, -fill => 'grey90', -tags => 'GRID') if $grid;
		$canvas->Subwidget('canvas')->lower('GRID');
	} else {
		$canvas->delete('GRID')
	}
}

#===============
sub FillCanvas {
#===============	
	my $c = $canvas;
	my $m = $self->{MATRIX} or return;
	my $z = $self->{ZOOM};
	my $l = $self->{LINE_WIDTH};
	my $lnc = $self->{LINE_COLOR};
	my $font1 = [
		$self->{FONT1}{FAMILY},
		$self->{FONT1}{SIZE}*$z ,
		$self->{FONT1}{WEIGHT},
		$self->{FONT1}{SLANT},
	];
	my $head1 = [
		$self->{FONT_HEAD}{FAMILY},
		$self->{FONT_HEAD}{SIZE}*$z ,
		$self->{FONT_HEAD}{WEIGHT},
		$self->{FONT_HEAD}{SLANT}
	];
	my $as = $self->{ALIVE_SPACE};
	my %save;
	
	CanvasTrimYdim();

	#### radiere alles hinfort ...
	$c->delete('all');

	ShowGrid();
	
	### Title		
	if (! $self->{TITLE_X}) {  
		($_) = sort { $a <=> $b } keys %{$m->{YX2P}} or return;
		@_ = sort { $a <=> $b } keys % { $m->{YX2P}{$_} } or return;
		$self->{TITLE_X} = ($_[0]+$_[-1])/2;
		$self->{TITLE_Y} = $_-3;
	}	
	
	if ($param->{SHOW_HEAD} ) {						
		if (! $self->{TITLE} && $self->{FAMILY}) {  $self->{TITLE} = $self->{FAMILY} }				
		$c->createText(
			$self->{TITLE_X}*$self->{GITTER_X}*$z, $self->{TITLE_Y}*$self->{GITTER_Y}*$z, 			
			-anchor => 'center', -text => $self->{TITLE} , 
			-font => $head1, -fill => $self->{FONT_HEAD}{COLOR}, -tags => [ 'TEXT' , 'HEAD', 'TAG' ]
		)
	}
		
	### Zeichnen aller Personen-bezogenen Elemente
	### Drawing of individual related elements
	my $sz = $self->{SYMBOL_SIZE}/2;
	foreach my $Y (keys % { $m->{YX2P} }) {
		foreach my $X (keys % { $m->{YX2P}{$Y} }) {
			my $p = $m->{YX2P}{$Y}{$X};
			if ($save{$p}) {next}
			$save{$p} = 1;
			my ($sex, $aff) = ( $self->{SID2SEX}{$p}, $self->{SID2AFF}{$p} );
			if (! defined $sex or ! defined $aff) {
				ShowInfo("Achtung : fehlende Daten zu Person $p !", 'error');
			}

			my ($cx, $cy) = ($X*$self->{GITTER_X}, $Y*$self->{GITTER_Y});
			my $col = $self->{AFF_COLOR}{$aff};

			### female
			if ($sex == 1) {
				$c->createRectangle(
					($cx-$sz)*$z, ($cy-$sz)*$z,
					($cx+$sz)*$z, ($cy+$sz)*$z ,
					-width => $l*$z, -outline => $lnc,
					-fill => $col, -tags => [ 'SYMBOL', "SYM-$p" , 'TAG' ] );
			}
			### male
			elsif ($sex == 2) {
				$c->createOval(
					($cx-$sz)*$z, ($cy-$sz)*$z,
					($cx+$sz)*$z, ($cy+$sz)*$z ,
					-width => $l*$z, -outline => $lnc,
					-fill => $col, -tags => [ 'SYMBOL', "SYM-$p", 'TAG' ]);
			}
			### other
			else {
				$c->createPolygon(
					($cx-$sz)*$z, $cy*$z,
					$cx*$z, ($cy-$sz)*$z,
					($cx+$sz)*$z, $cy*$z,
					$cx*$z, ($cy+$sz)*$z,
					-width => $l*$z, -outline => $lnc,
					-fill => $col, -tags => [ 'SYMBOL', "SYM-$p", 'TAG' ]);
			}
			### Fragezeichen bei unbekanntem Affection-status
			### question mark if affection status is unknown
			if ($self->{SHOW_QUEST} && ! $aff) {
				$c->createText(
					$cx*$z, $cy*$z,
					-anchor => 'center', -text => '?',
					-font => $font1, -fill => $self->{FONT1}{COLOR}, -tags => [ 'TEXT', "QUEST-$p", 'QUEST' ]
				);
			}
			
			### live status
			if (! $self->{SID2ALIVE}{$p}) {
				if ($self->{SID2AFF}{$p} == 0 && $self->{SHOW_QUEST}) {				
					$c->createLine(
						($cx-$sz-$as)*$z, ($cy+$sz+$as)*$z ,
						($cx-$sz+$as+1)*$z, ($cy+$sz-$as-1)*$z ,
						-width => $l*$z,-fill => $lnc, -tags => [ 'TOT' ]			
					);
					
					$c->createLine(
						($cx+$sz-$as-1)*$z, ($cy-$sz+$as+1)*$z ,
						($cx+$sz+$as)*$z, ($cy-$sz-$as)*$z ,
						-width => $l*$z,-fill => $lnc, -tags => [ 'TOT' ]			
					)					
				} else {
					$c->createLine(
						($cx-$sz-$as)*$z, ($cy+$sz+$as)*$z ,
						($cx+$sz+$as)*$z, ($cy-$sz-$as)*$z ,
						-width => $l*$z,-fill => $lnc, -tags => [ 'TOT' ]			
					)				
				}												
			}			
			
			### Personenbezeichner und Case Infos
			### Individual identifier and case information
			### changed 05/2007
			my $cc = 0;
			foreach my $col ( 1 .. 4 ) {	
				if ($self->{CASE_INFO_SHOW}{$col} && $self->{CASE_INFO}{COL_TO_NAME}{$col}) {
					my $yp = ($cy+$sz)*$z + $self->{FONT1}{SIZE}*$z + $cc*$self->{FONT1}{SIZE}*$z;
					my $name = $self->{CASE_INFO}{COL_TO_NAME}{$col};					
					my $text = $self->{CASE_INFO}{PID}{$p}{$name};
					$c->createText(
						$cx*$z,  $yp,
						-anchor => 'center', -text => $text ,
						-font => $font1, -fill => $self->{FONT1}{COLOR}, -tags => [ 'TEXT', "TEXT-$p", "TEXT-$p-$cc", 'TAG' ]
					);
					$cc++;
				}
			}
		}
	}
	
	
	@_ = $c->bbox('all');
	if ($param->{SHOW_DATE}) {				
		my @t = split ' ', localtime(time);
		$c->createText(
			$_[2]+25*$z,  $_[1]-80*$z,
			-anchor => 'ne', -text => "@t[0,1,2,4]",
			-font => $font1, -tags => [ 'TEXT', 'DATUM', 'TAG' ]
		);
	}	
	
	if ($param->{SHOW_PEDFILE} &&  $self->{PEDIGREE_PATH}) {
		$c->createText(
			$_[0]-150*$z,$_[3]+80*$z, -anchor => 'nw',
			 -text => "Pedfile: $self->{PEDIGREE_PATH}" , 
			-font => [ $self->{FONT1}{FAMILY}, 12*$z ]
		)		
	}
	
	if ($param->{SHOW_HAPLOFILE} &&  $self->{HAPLO_PATH}) {
		$c->createText(
			$_[0]-150*$z,$_[3]+80*$z+(14*$z), -anchor => 'nw',
			 -text => "Haplofile: $self->{HAPLO_PATH}" , 
			-font => [ $self->{FONT1}{FAMILY}, 12*$z ]
		)		
	}
	
}
	

#==============
sub DrawHaplo {
#==============	
	my $c = $canvas;
	my $m = $self->{MATRIX};
	my $h = $self->{HAPLO} or return;
	return unless keys %{$self->{HAPLO}{PID}};
	my $z = $self->{ZOOM};
	my $f1 = $self->{FONT1};
	my $fh = $self->{FONT_HAPLO};
	my $l = $self->{LINE_WIDTH};
	my $lnc = $self->{LINE_COLOR};
	my $lw = $self->{HAPLO_TEXT_LW};
	my $td1 = ($fh->{SIZE}*$z) + ($lw*$fh->{SIZE}*$z);
	my $font1  =  [ $f1->{FAMILY},$f1->{SIZE}*$z ,$f1->{WEIGHT},$f1->{SLANT} ];
	my $font_haplo =  [ $fh->{FAMILY},$fh->{SIZE}*$z , $fh->{WEIGHT}, $fh->{SLANT} ];
	my $head1 = [ $f1->{FAMILY}, $f1->{SIZE}*10*$z , $f1->{WEIGHT}, $f1->{SLANT} ];
	my $hw = $self->{HAPLO_WIDTH};
	my $hwni = $self->{HAPLO_WIDTH_NI};
	my $hs = $self->{HAPLO_SPACE};
	my $hlw = $self->{HAPLO_LW};
	my $un = $self->{HAPLO_UNKNOWN};		
	
	### find most left and right X matrix position for later legend drawing
	my @X_GLOB; push @X_GLOB,( keys % { $m->{YX2P}{$_} }) foreach keys % { $m->{YX2P} }; @X_GLOB = sort { $a <=> $b } @X_GLOB;

	### letzen gueltigen Index finden ($i2) und Anzahl zu zeichnender Elemente ($i3)
	my ($i1, $i2, $i3) = (0,0,0);
	foreach (@{$h->{DRAW}}) {
		if ($_) { $i2 = $i1 ; $i3++ } $i1++
	}

	### alle 'gesperrten' Farben finden
	my %Hide;
	if ($pedigree{$self->{FAMILY}}) {
		foreach (@{$pedigree{$self->{FAMILY}}}) {
			my $pid = @$_[0];
			for my $mp ( 'M','P') {
				if ($h->{PID}{$pid}{$mp}{HIDE}) {
					foreach ( @{$h->{PID}{$pid}{$mp}{BAR}}) {
						$Hide{@$_[1]} = 1 if @$_[1] ne $self->{HAPLO_UNKNOWN_COLOR}
					}
				}
			}
		}
	}


	### Zeichnen aller Personen-bezogenen Elemente
	my $sz = $self->{SYMBOL_SIZE}/2;
	foreach my $Y (keys % { $m->{YX2P} }) {
		my @X = sort { $a <=> $b } keys % { $m->{YX2P}{$Y} };
		my @bbox;
		foreach my $X (@X) {
			my $p = $m->{YX2P}{$Y}{$X};
			@bbox = $canvas->bbox("TEXT-$p", "SYM-$p");
			my ($cx, $cy) = ($X*$self->{GITTER_X}, $Y*$self->{GITTER_Y} + $lw*$z*2);

			### Haplotypen als BAR
			if ( $h->{PID}{$p}{P}{TEXT} ) {
				if ($self->{SHOW_HAPLO_BAR}) {
					my $td = $td1;
					my ($col, $inf, $ncol, $ninf, $out, $lr, $fill, $al, $x1, $x2, $y1, $y2 );

					### BAR wird auf Y_SPACE Niveau geschrumpft
					if (! BarTextOk()) {
						my $cc = 0; foreach (@ { $self->{SHOW_CASE} }) { $cc++ if $_ }
						$td = ((($self->{Y_SPACE}- 2.8 -$self->{Y_SPACE_EXTRA})*$self->{GITTER_Y}*$z)-($cc*$self->{FONT1}{SIZE}*$z))/$i3;
					}

					my $y = $bbox[3] + $self->{FONT1}{SIZE}*$z + $td;

					foreach my $PM ( 'P', 'M') {
						my ($f, $cc, $nif) = (1,0,0);
						if ($PM eq 'M') { $lr = -1 } else { $lr = 1 }

						for (my $i=0; $i <= $i2;$i++) {
							next unless $self->{HAPLO}{DRAW}[$i];
							$al = $h->{PID}{$p}{$PM}{TEXT}[$i];
							($inf,$col) = @{ $h->{PID}{$p}{$PM}{BAR}[$i] };
							next if $Hide{$col};

							### Bar als nicht-informativ zeichnen
							### NI-0: Genotypen des gesamten Haplotypen fehlen
							### NI-1: einzelner Genotyp ist ausgefallen
							### NI-2: Genotyp nicht ausgefallen +   'per Hand' als nicht-informativ deklariert
							### NI-3: Genotyp nicht ausgefallen +  automatisch als nicht informativ deklariert
							if (
								( ($inf eq 'NI-0') && $self->{SHOW_HAPLO_NI_0} ) ||
								( ($inf eq 'NI-1') && $self->{SHOW_HAPLO_NI_1} ) ||
								( ($inf eq 'NI-2') && $self->{SHOW_HAPLO_NI_2} ) ||
								( ($inf eq 'NI-3') && $self->{SHOW_HAPLO_NI_3} )
							) {
								$out = $fill = $self->{HAPLO_UNKNOWN_COLOR};
								if ( $self->{SHOW_HAPLO_TEXT} && ! $self->{ALLELES_SHIFT}) { $cc++; next }
								($x1, $x2) = ( ($cx-($lr*$hs)-($hwni/2))*$z, ($cx-($lr*$hs)+($hwni/2))*$z );
								$nif = 1;
							} else {
								$out = $fill = $col;
								($x1, $x2) = ( ($cx-($lr*$hs)-($hw/2))*$z, ($cx-($lr*$hs)+($hw/2))*$z );
							}

							undef $fill if ! $self->{FILL_HAPLO};

							if (! $self->{HAPLO_SEP_BL}) {
								if ($i != $i2) {
									### naechstes zu zeichnende Element
									my $i4; for ($i4=$i+1; $i4 <= $i2;$i4++) {last if $self->{HAPLO}{DRAW}[$i4]}
									($ninf,$ncol) = @{ $h->{PID}{$p}{$PM}{BAR}[$i4] };
									my $nexti = 0; if (
										( ($ninf eq 'NI-0') && $self->{SHOW_HAPLO_NI_0} ) ||
										( ($ninf eq 'NI-1') && $self->{SHOW_HAPLO_NI_1} ) ||
										( ($ninf eq 'NI-2') && $self->{SHOW_HAPLO_NI_2} ) ||
										( ($ninf eq 'NI-3') && $self->{SHOW_HAPLO_NI_3} )
									) { $nexti = 1 }

									if ( ($col eq $ncol) && ($nif == $nexti) ) {
										$f++; $cc++; next
									} else {
										($y1, $y2) = ( $y + ($cc-$f)*$td,  $y + $cc*$td ); $f = 1
									}
								} else {
									($y1, $y2) = ( $y + ($cc-$f)*$td,  $y + $cc*$td ); $f = 1
								}
							} else {
								($y1, $y2) = ( $y + ($cc-1)*$td,  $y + $cc*$td )
							}

							$_ = $c->createRectangle(
								$x1 , $y1, $x2 , $y2,
								-width => $hlw*$z, -outline => $out,
								-fill => $fill, -tags => [ "BAR", "BAR1-$p", 'TAG' ]
							);	$cc++;
							unless (defined $h->{PID}{$p}{$PM}{BAR}[$i][1] ) {
								$c->Subwidget('canvas')->raise($_, "SYM-$p");
							}
							if ($nif) { $c->Subwidget('canvas')->lower($_) ; $nif = 0 }
						}
					}
				}

				### Haplotypen als TEXT
				if ($self->{SHOW_HAPLO_TEXT}) {
					my $cc = 0;
					my $col;
					my $sh = $self->{ALLELES_SHIFT};
					my ($x1, $x2) = ( ($cx-$hs-$sh)*$z, ($cx+$hs+$sh)*$z );
					my $y = $bbox[3] + $self->{FONT1}{SIZE}*$z + $td1/2;
				
					### Paternaler Haplotyp
					for (my $i=0; $i <= $#{ $h->{PID}{$p}{P}{TEXT} };$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						my $inf = $h->{PID}{$p}{P}{BAR}[$i][0];
						
						if (
								$self->{SHOW_COLORED_TEXT} && ! (
								( ($inf eq 'NI-0') && $self->{SHOW_HAPLO_NI_0} ) ||
								( ($inf eq 'NI-1') && $self->{SHOW_HAPLO_NI_1} ) ||
								( ($inf eq 'NI-2') && $self->{SHOW_HAPLO_NI_2} ) ||
								( ($inf eq 'NI-3') && $self->{SHOW_HAPLO_NI_3} ) )
							) { $col = $h->{PID}{$p}{P}{BAR}[$i][1] } else { $col = $fh->{COLOR} }
														
						
						$h->{PID}{$p}{P}{TEXT}[$i] =~ s/@/$self->{HAPLO_UNKNOWN}/;
						$c->createText(
							$x1, $y+ ($cc*$td1),
							-anchor => 'center', -text => $h->{PID}{$p}{P}{TEXT}[$i] ,
							-font => $font_haplo, -fill => $col, -tags => [ 'ALLEL', "ALLEL-P-$i-$p" ]
						);
						$cc++
					}


					$cc = 0;
					### Maternaler Haplotyp
					for (my $i=0; $i <= $#{ $h->{PID}{$p}{M}{TEXT} };$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						my $inf = $h->{PID}{$p}{M}{BAR}[$i][0];
						if (
								$self->{SHOW_COLORED_TEXT} && ! (
								( ($inf eq 'NI-0') && $self->{SHOW_HAPLO_NI_0} ) ||
								( ($inf eq 'NI-1') && $self->{SHOW_HAPLO_NI_1} ) ||
								( ($inf eq 'NI-2') && $self->{SHOW_HAPLO_NI_2} ) ||
								( ($inf eq 'NI-3') && $self->{SHOW_HAPLO_NI_3} ) )
							) { $col = $h->{PID}{$p}{M}{BAR}[$i][1] } else { $col = $fh->{COLOR} }
						
						
						$h->{PID}{$p}{P}{TEXT}[$i] =~ s/@/$self->{HAPLO_UNKNOWN}/;
						$h->{PID}{$p}{M}{TEXT}[$i] =~ s/@/$self->{HAPLO_UNKNOWN}/;
						$c->createText(
							$x2, $y + ($cc*$td1),
							-anchor => 'center', -text => $h->{PID}{$p}{M}{TEXT}[$i],
							-font => $font_haplo, -fill => $col, -tags => [ 'ALLEL', "ALLEL-M-$i-$p" ]
						);
						$cc++
					}
				}


				### Haplotypen als Bounding Boxes
				if ($self->{SHOW_HAPLO_BBOX} && $h->{PID}{$p}{BOX}) {
					my ($x1, $x2) = (($cx-$self->{BBOX_WIDTH})*$z,($cx+$self->{BBOX_WIDTH})*$z);
					my ($y1, $y2);
					my $f = 1;
					my $cc = 0;
					my $td = $td1;
					if (! $self->{SHOW_HAPLO_TEXT} ) {
						$td = ($self->{Y_SPACE}-3.5)*$self->{GITTER_Y}*$z/$i3;
					}
					my $y = $bbox[3] + $self->{FONT1}{SIZE}*$z + $td;
					for (my $i=0; $i <= $i2;$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						unless ($h->{PID}{$p}{BOX}[$i]) { $cc++; next }
 						if ($i != $i2) {
							if ( $h->{PID}{$p}{BOX}[$i+1] ) {
								$f++; $cc++; next
							} else {
								($y1, $y2) = ( $y + ($cc-$f)*$td,  $y + $cc*$td ); $f = 1
							}
						} else {
							($y1, $y2) = ( $y + ($cc-$f)*$td,  $y + $cc*$td ); $f = 1
						}

						$c->createRectangle(
							$x1, $y1, $x2, $y2,
							-width => $hlw*$z, -outline => 'black',
							-tags => [ 'BOX', "BOX-$p", 'TAG' ]
						);	$cc++
					}
				}
			}
		}

		### Map Informationen										
		if (@X && $h->{MAP}) {
			my $cc = 0;
			my $y = $bbox[3] + $self->{FONT1}{SIZE}*$z + $td1/2;
			if ($self->{SHOW_MARKER}) {
				
				### Marker left side
				if ($self->{SHOW_LEGEND_LEFT}) {
					$cc = 0; 
					my $x;
					if ($self->{ALIGN_LEGEND}) {
						$x = ( ($X_GLOB[0]*$self->{GITTER_X}) - $self->{LEGEND_SHIFT_LEFT} ) * $z;
					} else {
						$x = ( ($X[0]*$self->{GITTER_X}) - $self->{LEGEND_SHIFT_LEFT} ) * $z;
					}																	 
					for (my $i=0; $i <= $#{ $h->{MAP}{MARKER} };$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						$c->createText(
							$x, $y + ($cc*$td1),
							-anchor => 'w', -text => $h->{MAP}{MARKER}[$i] ,
							-font => $font_haplo, -fill => $fh->{COLOR}
						);
						$cc++
					}
				}
				
				### Marker right side
				if ($self->{SHOW_LEGEND_RIGHT}) {
					$cc = 0; 
					my $x;
					if ($self->{ALIGN_LEGEND}) {
						$x = ( ($X_GLOB[-1]*$self->{GITTER_X}) + $self->{LEGEND_SHIFT_RIGHT} ) * $z;
					} else {
						$x = ( ($X[-1]*$self->{GITTER_X}) + $self->{LEGEND_SHIFT_RIGHT} ) * $z;
					}																	 
					for (my $i=0; $i <= $#{ $h->{MAP}{MARKER} };$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						$c->createText(
							$x, $y + ($cc*$td1),
							-anchor => 'w', -text => $h->{MAP}{MARKER}[$i] ,
							-font => $font_haplo, -fill => $fh->{COLOR}
						);
						$cc++
					}
				}
			
			
			}

			if ($self->{SHOW_POSITION}) {				
				
				### Position left side
				if ($self->{SHOW_LEGEND_LEFT}) {
					$cc = 0; 
					my $x;
					if ($self->{ALIGN_LEGEND}) {
						$x = ( ($X_GLOB[0]*$self->{GITTER_X}) - $self->{LEGEND_SHIFT_LEFT} + $self->{MARKER_POS_SHIFT} ) * $z;
					} else {
						$x = ( ($X[0]*$self->{GITTER_X}) - $self->{LEGEND_SHIFT_LEFT} + $self->{MARKER_POS_SHIFT} ) * $z;
					}

					for (my $i=0; $i <= $#{ $h->{MAP}{POS} };$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						$c->createText(
							$x, $y + ($cc*$td1),
							-anchor => 'e', -text => sprintf("%6.2f",$h->{MAP}{POS}[$i]) ,
							-font => $font_haplo, -fill => $fh->{COLOR}
						);
						$cc++
					}													
				}
				
				### Position right side
				if ($self->{SHOW_LEGEND_RIGHT}) {
					$cc = 0; 
					my $x;
					if ($self->{ALIGN_LEGEND}) {
						$x = ( ($X_GLOB[-1]*$self->{GITTER_X}) + $self->{LEGEND_SHIFT_RIGHT} + $self->{MARKER_POS_SHIFT} ) * $z;
					} else {
						$x = ( ($X[-1]*$self->{GITTER_X}) + $self->{LEGEND_SHIFT_RIGHT} + $self->{MARKER_POS_SHIFT} ) * $z;
					}

					for (my $i=0; $i <= $#{ $h->{MAP}{POS} };$i++) {
						next unless $self->{HAPLO}{DRAW}[$i];
						$c->createText(
							$x, $y + ($cc*$td1),
							-anchor => 'e', -text => sprintf("%6.2f",$h->{MAP}{POS}[$i]) ,
							-font => $font_haplo, -fill => $fh->{COLOR}
						);
						$cc++
					}													
				}
				
			}

		}
	}
	$canvas->Subwidget('canvas')->lower('GRID');
}

# Aligning could be improved
#================
sub AlignMatrix {
#================	
	my ($steps) = @_;
	my $s = $self->{STRUK};
	my $m = $self->{MATRIX};
	my @s;
	my $cc = 1;
	my $cd = 0;
	my $ok = 1;

	foreach my $Y ( sort { $b <=> $a } keys % { $m->{YX2P} } ) {
		my %Save;
		foreach my $X ( sort { $a <=> $b } keys % { $m->{YX2P}{$Y} } ) {
			my $P = $m->{YX2P}{$Y}{$X} or die "No Person in XY $X $Y\n", Dumper($m);
			my ($fa,$mo) = ($self->{SID2FATHER}{$P},$self->{SID2MOTHER}{$P});
			next if ! $fa && ! $mo;
			
			### Geschwister von $P einschliesslich $P
			@s = keys %{$self->{CHILDREN_COUPLE}{$fa}{$mo}};
			
			my $str; $str .= $_ for @s;
			next if $Save{$str}; $Save{$str} = 1;

			### alle X Koordinaten der Geschwister
			my %k; foreach (@s) { $k{ $m->{P2XY}{$_}{X} } = $_	}
			my @sk = sort { $a <=> $b } keys %k;

			### im Falle multipler Gatten muss noch in 'richtige' Zeichenposition uebersetzt werden
			### code neu 12/2006 V.029b
			if ( ( scalar keys % { $self->{COUPLE}{$fa} } > 1 ) or ( scalar keys % { $self->{COUPLE}{$mo} } > 1 ) ) {				
				my ($G, $S, $PP) = FindAdress($fa);								
				my $CC=0; foreach my $i ( @ { $s->[$G][$S][$PP][1] } ) {
					if ( (($i->[0] eq $fa) and ($i->[1] eq $mo)) or (($i->[0] eq $mo) and ($i->[1] eq $fa)) ) {
						($fa, $mo) = @ { $s->[$G][$S][$PP][2][$CC] }; 
						last
					}
					$CC++;
				}
			}

			my $Y_f = $m->{P2XY}{$fa}{Y};
			my $kf = $m->{P2XY}{$fa}{X};
			my $km = $m->{P2XY}{$mo}{X};
			my %k2 = ( $kf => $fa, $km => $mo);
			my @ek =  sort { $a <=> $b } keys %k2;
			my $mitte_c = sprintf("%1.0f", ($sk[0]+$sk[-1])/2);
			my $mitte_e = sprintf("%1.0f", ($kf+$km)/2);
			my $diff = $mitte_c-$mitte_e;

			if ($steps) {return if $cc >= $steps }

			my $ind = 0;
			my $newpos1 = $sk[0]-$diff;
			my $newpos2 = $ek[0]+$diff;

			if (scalar (keys %{ $self->{COUPLE}{$k2{$ek[0]}}}) != 1) {
				$ind = 1;
				$newpos2 = $ek[1]+$diff;
			}

			if ( $diff < 0 ) {
				### Shift Kinder nach rechts ->
				ShiftRow($Y, $k{$sk[0]}, $newpos1);
				$self->{PID_SAVE}{$k2{$ek[0]}} = 1;
				return 0
			}

			elsif ( $diff > 0 )  {
				### Shift Eltern nach rechts ->
				unless (ShiftRow($Y_f, $k2{$ek[$ind]}, $newpos2,1)) {next};
			 	return  0

			}
			$cc++;
		}
	}
	return $cc;
}

# Row Shift rechts: Shift erfolgt 'gleitend' d.h. Luecken werdend waehrend des
# shifts aufgefuellt
#=============
sub ShiftRow {
#=============	
	my ($Y, $pid, $NewPos, $flag) = @_;
	my $m = $self->{MATRIX};
	my $OldPos = $m->{P2XY}{$pid}{X};
	return if $NewPos == $OldPos;
	my (%SaveRow, %Freeze);

	### Wird benoetigt um sich kreuzende Zeichengruppen zu erkennen (shift wird unterbunden)
	foreach my $P (keys % { $self->{PID_SAVE} }) {
		next if $pid eq $P;
		next if $m->{P2XY}{$P}{Y} != $Y;
		my $X = $m->{P2XY}{$P}{X};
		$Freeze{$X} = $P if $X >= $OldPos;
	}
	(my $XL) = sort { $a <=> $b } keys %Freeze;

	if ($flag && $XL && ( $NewPos >= $XL) ) { return undef }

	foreach my $X (sort { $a <=> $b } keys % { 	$m->{YX2P}{$Y} } ) {
		$SaveRow{$m->{YX2P}{$Y}{$X}} =  $X ;
	}

	foreach my $st ( $OldPos .. $NewPos-1 ) {
		my (@right, @pid);
		foreach my $X (sort { $a <=> $b } keys % { 	$m->{YX2P}{$Y} } ) {
			if ($X >= $OldPos) {
				push @right, $X;
				push @pid, $m->{YX2P}{$Y}{$X}
			}
		}
		for (my $i = 0; $i <= $#right; $i++) {
			my $X = $right[$i];
			my $P = $pid[$i];
			delete $m->{YX2P}{$Y}{$X};
			$X++;
			$m->{YX2P}{$Y}{$X} = $P;
			$m->{P2XY}{$P}{X}  = $X;
			if ($right[$i+1]) {
				last if $right[$i+1]-$X-1 >= $self->{X_SPACE}
			}
		}
	}
	return 1;
}


# Berechnung aller relevanter Linienkoordinaten und checkt Kreuzungen, Rueckgabewert ist deren Anzahl
# alle Koordinaten in $self->{LINES} fuer Methode DrawLines()
#=============
sub SetLines {
#=============	
	my $cr = 0;							### Zaehler fuer Kreuzungen
	my $c = $canvas;
	my $z = $self->{ZOOM};
	$self->{LINES} = {};
	my $d = $self->{LINES};
	my $s = $self->{STRUK};
	my $gy = $self->{GITTER_Y};
	my $cf1 = $self->{CROSS_FAKTOR1};
	my $cl  = $self->{CROSS_LOOP};
	my $cd  = $self->{CONSANG_DIST};
	my $f   = $cl*$z;					### bestimmt Groesse der Kreuzungs-Schleife	
	
	### Phase 1: erst mal alle Linien-Koordinanten bestimmen
	### 1. Linien zwischen den Eltern berechnen
	my $CG = 0; foreach my $G (@$s) {
		my $CS = 0; foreach my $S (@$G) {
			my $CP = 0; foreach my $P (@$S) {
				if ( ref $P ) {									
					my $CC = 0; foreach my $C ( @{$P->[1]} ) {
						my ($c1, $c2) = @$C;
						$d->{COUPLE}{"$CG$CS$CP$CC"}{PID} = [ $c1, $c2 ];
						
						### 12/06 code ersetzt durch folgenden!
						### Koordinaten der 'zechnerisch korrekten' Eltern und nicht der wirklichen													
						my @c1 = $c->coords("SYM-$P->[2][$CC][0]") or ( warn "Person $c1 fehlt !" , next );
						my @c2 = $c->coords("SYM-$P->[2][$CC][1]") or ( warn "Person $c2 fehlt !",  next );
												
						if ($c1[4]) { @c1[2,3] = @c1[4,5] }
						if ($c2[4]) { @c2[2,3] = @c2[4,5] }

						my (@X1, @X2);
						my ($x1, $x2);

						@X1 = @c1[0,2];
						@X2 = @c2[0,2];

 						if ($X1[0] < $X2[0]) { ($x1, $x2) = ( $X1[1], $X2[0] ) }
 						else { ($x1, $x2) = ( $X1[0], $X2[1] ) }

						my $xm1 = ($X1[0]+$X1[1])/2;
						my $xm2 = ($X2[0]+$X2[1])/2;
						my $y = ($c1[1]+$c1[3])/2;					
						
						### double line in case of consanguinity
						if (  $self->{LOOP}{CONSANGUINE}{$c1}{$c2} ) {
								$d->{COUPLE}{"$CG$CS$CP$CC"}{POS} =
							[
								[ $xm1, $y+$cd*$z, $xm2, $y+$cd*$z ],
							 	[ $xm1, $y-$cd*$z, $xm2, $y-$cd*$z ]
							]												
						} else {
							$d->{COUPLE}{"$CG$CS$CP$CC"}{POS} = [ [ $x1, $y, $x2, $y ] ]
						} $CC++
					} $CP++
				} $CS++
			} $CG++
		}
	}


	### 2. Linien zwischen SIBS berechnen (falls keine Einzelkinder)
	foreach my $id (keys %{$d->{COUPLE}}) {
		my ($c1, $c2) = @ { $d->{COUPLE}{$id}{PID} };
		my @children = keys %{$self->{CHILDREN_COUPLE}{$c1}{$c2}};
		if (scalar @children > 1) {
			my (@x, $yc, $y1, @cy, %ch);
 			my $r = $d->{SIB}{$id} = [];

 			### Y-Koordinaten sortieren
 			foreach (@children) {		
 				my @co = $c->coords("SYM-$_") or ( warn "Person $_ fehlt - !" , next );
 				if ($co[4]) { @co[2,3] = @co[4,5] }
 				my $xm = sprintf("%1.3f", ($co[0]+$co[2])/2); 
 				### work around to prevent overwriting children with same x coordinate
 				### this is in special loops where individuals occur in different generations
 				### 14/05/07
 				if ($ch{$xm}) { $xm+= 0.001 }				
 				my $ym = sprintf("%1.3f", ($co[1]+$co[3])/2);
 				push @cy, $ym;
 				$ch{$xm}{CHILD} = $_;
 				$ch{$xm}{YM} = $ym;
 				$ch{$xm}{COOR} = \@co;				 				
 			}
						
			
 			### nach X-Koordinate sortierte Liste einer Kindergruppe
 			my @child_x = sort { $a <=> $b } keys %ch;
 									
 			@cy = sort { $a <=> $b } @cy;
			my $K_F = shift @child_x;
			my $K_L = pop @child_x;	
			

			my ($xa1, $ya1, $xa2, $ya2, $xa3, $ya3, $xa4, $ya4) = (
				$K_F, $ch{$K_F}{COOR}[1],
				$K_F, $cy[0] - ($self->{GITTER_Y}*$z),
				$K_L, $cy[0] - ($self->{GITTER_Y}*$z),
				$K_L, $ch{$K_L}{COOR}[1]
			);
                      
			push @$r, [ $xa1, $ya1, $xa2, $ya2, $xa3, $ya3, $xa4, $ya4 ];			
			
			foreach my $xm (@child_x) {
				push @$r, [ $xm, $ch{$xm}{COOR}[1], $xm, $cy[0]-($self->{GITTER_Y}*$z) ]
			}
			
		}
		### Einzelkind
		else {
			my @co = $c->coords("SYM-$children[0]") or ( warn "Person $_[0] fehlt !" , next );
			if ($co[4]) { @co[2,3] = @co[4,5] }
			$d->{CHILD}{$id} = [ ($co[0]+$co[2])/2, $co[1] ]
		}
	}

	#### 3. Linien zwischen Eltern und SIBS berechnen
	foreach my $id (keys %{$d->{COUPLE}}) {
		my $r = $d->{COUPLE}{$id}{POS} or next;
		my ($x1, $x2, $y1) = ( $r->[0][0], $r->[0][2], $r->[0][1]);
		my ($xm1, $xd1) = ( ($x1+$x2)/2, $x2-$x1 );


		### Es gibt eine SIB-Gruppe
		if ($d->{SIB}{$id}) {
			my ($x3, $x4, $y2) = ( $d->{SIB}{$id}[0][2], $d->{SIB}{$id}[0][4], $d->{SIB}{$id}[0][3] );
			my ($xm2,$xd2) = ( ($x3+$x4)/2, $x4-$x3 );

			### Unterteilung der Verbindung noetig, da Gruppen nicht untereinander stehen
			if ( ($x3 > $x2) || ($x1 > $x4) ) {
				$d->{COUPLE_SIB}{$id} =
					[
						$xm1, $y1,
						$xm1, $y2-($cf1*$gy*$z),
						$xm2, $y2-($cf1*$gy*$z),
						$xm2, $y2
					]
			}


			### Direkte Verbindung in Abhaengigkeit welche Gruppe 'breiter' ist
			else {
				if ( $xd1 <= $xd2  ) {
					$d->{COUPLE_SIB}{$id} = [ $xm1, $y1, $xm1, $y2 ]
				} else {
					$d->{COUPLE_SIB}{$id} = [ $xm2, $y1, $xm2, $y2 ]
				}
			}
		}
		### Einzelkind
		elsif ($d->{CHILD}{$id}) {
			my ($x3, $y2) = ($d->{CHILD}{$id}[0],  $d->{CHILD}{$id}[1]);

			### direkte Verbindung Eltern -> Einzelkind
			if ( ($x1 < $x3) && ($x2 > $x3) ) {
				$d->{COUPLE_SIB}{$id} = [ $x3, $y1, $x3, $y2 ]
			}
			### Unterteilung noetig
			else {
				$d->{COUPLE_SIB}{$id} =
					[
						$xm1, $y1,
						$xm1, $y2-($cf1*$gy*$z),
 						$x3,  $y2-($cf1*$gy*$z),
 						$x3,  $y2
					]
			}
		} else { warn "????????, wo ist Kind ?????????\n" }
	}

	### Phase 2: Suche nach Kreuzungen

	### 1. Kreuzungen zwischen Eltern/Kind -> Eltern/Kind
	### Alle Linien gegeneinander testen ...
	foreach my $id1 (keys %{$d->{COUPLE_SIB}}) {
		my $r1 = $d->{COUPLE_SIB}{$id1} or next;
		foreach my $id2 (keys %{$d->{COUPLE_SIB}}) {
			my $r2 = $d->{COUPLE_SIB}{$id2};
			next if $id1 == $id2;
			$cr += CrossCheck($r1, $r2);
			$cr += CrossCheck($r2, $r1);
		}

		foreach my $id3 (keys %{$d->{SIB}}) {
			my $r3 = $d->{SIB}{$id3}[0];
			$cr += CrossCheck($r1, $r3);
			$cr += CrossCheck($r3, $r1);
		}
		foreach my $id4 (keys %{$d->{COUPLE}}) {
			my $r4 = $d->{COUPLE}{$id4}{POS}[0];
			$cr+=  CrossCheck($r1, $r4);
			$cr+=  CrossCheck($r4, $r1);
		}
	}

	### Sonderfall -> lineare Ueberschneidung zwischen zwei Sibgruppen
	### in dem Fall Absetzung der Linien um bestimmten Betrag
	foreach my $id1 (keys %{$d->{SIB}}) {
		my $A = $d->{SIB}{$id1}[0];
		my $C = $d->{COUPLE_SIB}{$id1};
		foreach my $id2 (keys %{$d->{SIB}}) {
			my $B = $d->{SIB}{$id2}[0];
			next if $id1 == $id2;
			next if ! ($A->[3] == $B->[3]);
			next if ! 	(($A->[2] < $B->[2]) && ($A->[4] > $B->[2])) ||
						(($B->[2] < $A->[2]) && ($B->[4] > $A->[2]));
						
			$A->[3]  -= 6 * $z;
			$A->[5]  -= 6 * $z;
			$C->[-1] -= 6 * $z;

			@_ = @ {  $d->{SIB}{$id1} }; shift;
			$_->[3] -= 6 * $z foreach @_;
			
			$cr++;
		}
	}

	### Seltene Kreuzung Elternlinie -> SIB-Gruppe (nur bei Intergenerationsheiraten moeglich)
	foreach my $id1 (keys %{$d->{SIB}}) {
		foreach my $r1 ( @{ $d->{SIB}{$id1} }) {
			foreach my $id2 (keys %{$d->{COUPLE}}) {
				my $r2 = $d->{COUPLE}{$id2}{POS}[0];
				$cr+=  CrossCheck($r1, $r2);
				$cr+=  CrossCheck($r2, $r1);
			}
			foreach my $id3 (keys %{$d->{SIB}}) {
				foreach my $r3 ( @{ $d->{SIB}{$id3} }) {
					$cr+=  CrossCheck($r1, $r3);
					$cr+=  CrossCheck($r3, $r1);
				}
			}
		}
	}
	return $cr;
}


#===============
sub CrossCheck {
#===============
	my ($r1, $r2) = @_;
	### bestimmt Groesse der Kreuzungs-Schleife                                                                                                                                                                          
	my $f  = $self->{CROSS_LOOP} * $self->{ZOOM};
	my @A =  SplitLine($r1);
	my @B =  SplitLine($r2);
	my ($c, $a) = (0,1);
	my %A = ();

	foreach my $A (@A) {
		foreach my $B (@B) {
			###  A(|)   B(-)
			if ( ($A->[0] == $A->[2]) and ($B->[1] == $B->[3]) ) {
				if (	(($A->[0] > $B->[0]) && ($A->[0] < $B->[2])) &&
						(($A->[1] < $B->[1]) && ($A->[3] > $B->[1])) ) {
				 	$A{$a} =
				 		[
				 			$A->[0],	$A->[1],
				 			$A->[0],	$B->[1]-$f,
				 			$A->[0]+$f,	$B->[1]-$f,
				 			$A->[0]+$f,	$B->[1]+$f,
				 			$A->[0],	$B->[1]+$f,
				 			$A->[0],	$A->[3]
				 		];
					$c++
				}
				elsif (	(($A->[2] > $B->[0]) && ($A->[2] < $B->[2])) &&
						(($A->[3] < $B->[1]) && ($A->[1] > $B->[1])) ) {

					$A{$a} =
						[
				 			$A->[0],	$A->[1],
				 			$A->[0],	$B->[1]+$f,
				 			$A->[0]+$f,	$B->[1]+$f,
				 			$A->[0]+$f,	$B->[1]-$f,
				 			$A->[0],	$B->[1]-$f,
				 			$A->[0],	$A->[3]
				 		];

					$c++
				}
			}
		}
		$a++
	}
	for my $n ( 1 .. scalar @A ) {
		if (! $A{$n} ) {  push @{$A{$n}}, $_ foreach @{ $A[$n-1] } }
	}
	JoinLine($r1, \%A);
	return $c
}

#==============
sub SplitLine {
#==============	
	my $r = shift;
	my @r;
	for (my $i = 0; $i < scalar @$r-2; $i+=2) {
		push @r, [ @$r[$i], @$r[$i+1], @$r[$i+2], @$r[$i+3] ]
	}
	return @r
}

#=============
sub JoinLine {
#=============	
	my ($r, $h) = @_;
	@$r = ();
	foreach my $n ( sort { $a <=> $b } keys %$h ) {
		if (! ($n == 1) ) {  shift  @{ $h->{$n} };  shift  @{ $h->{$n}} }
		push @$r,  @{ $h->{$n} }
	}
}

#==============
sub DrawLines {
#==============	
	my $d = $self->{LINES};
	my $z = $self->{ZOOM};
	my $l = $self->{LINE_WIDTH};
	my $c = $canvas;
	my $lnc = $self->{LINE_COLOR};

	### Eltern Zeichnen - keine Ueberkreuzungen moeglich
	foreach my $id (keys %{$d->{COUPLE}}) {
		foreach my $ln ( @{$d->{COUPLE}{$id}{POS}} ) {
			$c->createLine( @$ln, -width => $l*$z, -fill => $lnc, -tags => [ 'LINE', 'PARENT-LINE', 'TAG'])
		}
	}

	### SIBs Zeichnen
	foreach my $id (keys %{$d->{SIB}}) {
		foreach my $ln ( @{$d->{SIB}{$id}} ) {
			$c->createLine( @$ln, -width => $l*$z, -fill => $lnc, -tags => [ 'LINE', 'SIB-LINE', 'TAG'])
		}
	}

	### Zwischenlinien Zeichnen
	foreach my $id (keys %{$d->{COUPLE_SIB}}) {
		$c->createLine( @{$d->{COUPLE_SIB}{$id}}, -fill => $lnc, -width => $l*$z, -tags => [ 'LINE', 'COUPLE-SIB-LINE', 'TAG'])
	}

	$c->Subwidget('canvas')->lower('LINE');
	$c->Subwidget('canvas')->lower('GRID');
}


# examination of all mates and recursive mates of mates of child $child
# the list of mates is represented as simple drawing order @S
# and couples list @D as derived from @S
# this sub is implemented new in V.024b
#===============
sub SetCouples {
#===============	
	my ($child) = shift;
	my (@S, @D ,@D2, %P, $flag, %SAVE);
	
	## find everybody joined in couple group  
	foreach ( keys % { $self->{COUPLE}{$child} }) {
		$P{$_} = 1  if ! $self->{CHILDREN}{$child}{$_}
	}
	W:while (1) {
		undef $flag;
		foreach my $p ( keys %P ) {
			foreach my $c ( keys % { $self->{COUPLE}{$p} }) {
				if (! $P{$c} && ! $self->{CHILDREN}{$p}{$c}) {
					$P{$c} = 1; $flag = 1
				}
			}
		}
		last W unless $flag
	}
	
	### @S is drawing order of multiple mates in string form as ( p1, p2, p3, p4 )
	@S = keys %P;
	$self->{STORE_DRAWN}{$_} = 1 foreach @S;
		
	return $child unless @S;
		
	ChangeOrder(\@S);			
	
	### from @S derived order of couples for example (  [ p1, p3 ], [ p2, p3 ], [ p3, p4 ] )
	### list @S is screened for most right hand free mate
	foreach my $p1 (@S) {
		foreach my $p2 (@S) {
			if ($self->{CHILDREN_COUPLE}{$p1}{$p2} && ! $SAVE{$p1}{$p2} && ! $SAVE{$p2}{$p1}) {
				push @D, [ $p1, $p2 ];
				$SAVE{$p1}{$p2} = 1
			}
		}		
	}
	
	### code included 12/2006
	### additionaly store mate order in respect of centering children corect
	if ($#S > 1) {
		foreach my $i (0 .. $#S-1) {
			push @D2, [ $S[$i], $S[$i+1] ];
		}
	}
	else {
		push @D2, [ $S[0], $S[1] ]
	}
	return [ [ @S ] , [ @D ], [ @D2] ];
}


# abhaenging vom Zeichnen von Haplotypen muss Y-Dimension des Canvas
# neu kalkuliert werden
#===================
sub CanvasTrimYdim {
#===================
	my $h = $self->{HAPLO};
	my $lw = $self->{HAPLO_TEXT_LW};
	my $z = $self->{ZOOM};
	my $es = $self->{Y_SPACE_EXTRA};
	
	my $cc = 0; foreach (@ { $self->{SHOW_CASE} }) { $cc++ if $_ }
	my $te = $self->{FONT1}{SIZE}*$z*$cc;

	### Wenn es Haplotypen TEXT gibt so muss die GITTER_Y Variable angepasst werden
	### fuer BARS wird der BAR auf die voreingestellte Y_SPACE_DEFAULT Variable gepresst
	if ( BarTextOk() ) {

		my $c = 0; foreach ( @{ $h->{DRAW} } ) { $c++ if $_ }
		my $td = ($self->{FONT_HAPLO}{SIZE}*$z) + ($lw*$self->{FONT_HAPLO}{SIZE}*$z);
		my $ys = sprintf("%1.0f", (($c*$td)+$te)/($self->{GITTER_Y}*$z))+3+$es;

		if ($self->{Y_SPACE} != $ys) {
			 TrimIt($self->{Y_SPACE}- $ys);
			 $self->{Y_SPACE} = $ys; 
		}
	}

	else {
		my $td = $self->{FONT1}{SIZE}*$z*$cc;
		my $ys = sprintf("%1.0f", $td/($self->{GITTER_Y}*$z))+5;
				
		if ( $self->{Y_SPACE_DEFAULT} != $self->{Y_SPACE} ) { 
			TrimIt($self->{Y_SPACE}-$self->{Y_SPACE_DEFAULT});
			$self->{Y_SPACE} = $self->{Y_SPACE_DEFAULT};  
		}						
	}
}

#===========
sub TrimIt {
#===========	
	my $diff = shift;
	my $m = $self->{MATRIX};
	my %t;

	### �bersetzungshash bauen
	my $c = 0; foreach my $Y (sort { $a <=> $b } keys %{ $m->{YX2P} }) {
		$t{$Y} = $Y - $c*$diff; $c++
	}

	### Y anpassen
	$m->{YX2P} = {};
	foreach my $p (keys % { $m->{P2XY} } ) {
		my ($x,$y) = ($m->{P2XY}{$p}{X} , $t{$m->{P2XY}{$p}{Y}});
		$m->{P2XY}{$p}{Y} = $y;
		$m->{YX2P}{$y}{$x} = $p;
	}
}

# BARS having additionally labels like marker and positions ?
#==============
sub BarTextOk {
#==============
	my $h = $self->{HAPLO};
	if ( (keys %{$h->{PID}} && $self->{SHOW_HAPLO_TEXT})  ||
	     ($h->{MAP}{MARKER} && @{$h->{MAP}{MARKER}} && $self->{SHOW_MARKER})  ||
	     ($h->{MAP}{POS} 	&& @{$h->{MAP}{POS}} 	&& $self->{SHOW_POSITION}) ) {
	     	
		return 1
	} else { return 0 }	
}


# wie der Name schon sagt ...
#=============
sub ShowHelp {
#=============
	my $help = $mw->Toplevel(-title => "HaploPainter Help");	
	my $scr_x  = $mw->screenwidth;
	my $scr_y  = $mw->screenheight;
	my $mw_szx = 0.7;
	my $mw_szy = 0.6;

	$help->geometry (
		int($scr_x*$mw_szx) . 'x' . int($scr_y * $mw_szy) .  '+' .
		int($scr_x*(1-$mw_szx)/2) . '+' . int($scr_y * (1-$mw_szy)/3)
	);
	
	my $t = $help->Scrolled('Text',
		-scrollbars => 'osoe',
		-font => [ $self->{FONT1}{FAMILY}, 14, 'bold' ],
		-bg => 	'#f4e3aa', -padx => 20, -pady => 20, -wrap => 'word', -relief => 'solid'
	)->pack(-fill => 'both', -expand => 1);
	
	unless ($param->{HELP}) { while (<DATA>) {
		chomp;
		$param->{HELP} .= "$_\n" 
		} 
	}
	
	$t->insert('end',$param->{HELP});
	$t->configure(-state=>'disabled');
	$help->idletasks; 
 	$help->iconimage($help->Photo(-format => 'gif', -data => GetIconData()));	
}

#================
sub GetIconData {
#================
<<EOD;
	R0lGODdhIAAgACIAACwAAAAAIAAgAIK15fwvLy/9IyRGRv///wAAAAAAAAAAAAADkgi63P4wyvmE
	tWxoTd3Fy8Z1zCdkIll+6KYuZju+cZi+QK2IAxX8wKBwKHQQj8ifMbC6MAhQKEPZoCp0gKh0YeUy
	YayFljD9lpsgxfhcNefCai17/naK5d4l+nSP0hVdWGt5bXtPeIBuiYZ9W4ttSZFAS5KSFFg8OHVp
	AJk4mDc0cJ2hKqAun6OeonY7pZqwsRQJADs=
EOD
} 


#============
sub Default {
#============
	my $arg = shift;
	
	### List of parameters, considered as default values
	@_ = qw /
	AFF_COLOR SHOW_QUEST LINE_COLOR BACKGROUND CROSS_FAKTOR1 CONSANG_DIST ALIVE_SPACE
	GITTER_X GITTER_Y SYMBOL_SIZE FONT1 FONT_HAPLO FONT_HEAD SHOW_CASE  CASE_HEAD_ROW
	ZOOM LINE_WIDTH X_SPACE Y_SPACE Y_SPACE_EXTRA Y_SPACE_DEFAULT CROSS_LOOP 
	MARKER_POS_SHIFT ALLELES_SHIFT HAPLO_UNKNOWN HAPLO_UNKNOWN_COLOR HAPLO_TEXT_LW SHOW_HAPLO_TEXT
	SHOW_HAPLO_BAR SHOW_HAPLO_NI_0 SHOW_HAPLO_NI_1 SHOW_HAPLO_NI_2 SHOW_HAPLO_NI_3 HAPLO_SEP_BL
	FILL_HAPLO HAPLO_WIDTH HAPLO_WIDTH_NI HAPLO_SPACE HAPLO_LW SHOW_MARKER SHOW_POSITION 
	SHOW_DATE SHOW_HEAD SHOW_HAPLO_BBOX BBOX_WIDTH TITLE_X TITLE_Y SHOW_HAPLOFILE SHOW_PEDFILE	
	SHOW_LEGEND_LEFT  SHOW_LEGEND_RIGHT  ALIGN_LEGEND  SHOW_COLORED_TEXT 
	LEGEND_SHIFT_LEFT LEGEND_SHIFT_RIGHT CASE_INFO_SHOW/;
	
	### updates defaults from $self
	if ($arg eq 'update') {			
		foreach (@_) {				
			if (ref $self->{$_}) { 
				$param->{DEFAULT}{$_} = dclone($self->{$_})
			}
			else { $param->{DEFAULT}{$_} = $self->{$_} }
		}
	}
	
	### restores $self from defaults
	elsif ($arg eq 'restore') {			
		foreach (@_) {				
			if (ref $param->{DEFAULT}{$_}) { 
				$self->{$_} = dclone($param->{DEFAULT}{$_});
			}
			else { $self->{$_} = $param->{DEFAULT}{$_} }
		}
	}
	
	### saving default file
	elsif ($arg eq 'save') {			
		$_ = $mw->getSaveFile(
			-initialfile 	=> 'my_haplopainter_defaults.hpd',
			-defaultextension	=> 'hpd',
			-filetypes		=> [
									[ 'All Files',	 '*' ],
									[ 'HaploPainter Defaults', 'hpd' ]
								]
		) or return undef;		
		store $param->{DEFAULT}, $_;
	}
	
	### open default file
	elsif ($arg eq 'open') {			
		$_ = $mw->getOpenFile(
			-filetypes		=> [
									[ 'All Files',	 '*' ],
									[ 'HaploPainter Defaults', 'hpd' ]
								]
		) or return undef;		
		$param->{DEFAULT} = retrieve($_);
		foreach (@_) {				
			if (ref $param->{DEFAULT}{$_}) { $self->{$_} = dclone($param->{DEFAULT}{$_})	}
			else { $self->{$_} = $param->{DEFAULT}{$_} }
		}
		RedrawPed();
		AdjustView();
	}
}

# Preparation for later batch mode processing. There are still no active Tk Elements.
# Used for early argument error checking 
#=================
sub PrepareBatch {
#=================
	my $path = shift @ARGV;
	my %arg;
	if ($path =~ /-h|-help|help|\?|-\?/i) { BatchError(); exit }
	open (FH, "<", $path) or die ($!);
		while (<FH>) {
			chomp;
			next unless $_;			
			next if /^\#/;
			@_ = split '=', $_;
			foreach (@_) { $_ =~ s/^\s+//g; s/\s+$//g }
			next if scalar @_ != 2;
			$arg{$_[0]} = $_[1];
		}			
	close FH;

	return undef unless %arg;
		
	
	foreach (qw /BORDER_UP BORDER_DOWN BORDER_LEFT BORDER_RIGHT GRAPHIC_FORMAT RESOLUTION
	             EXPORT_PATH PNGALPHA_COLOR TEXT_ALPHA_BITS GRAPHICS_ALPHA_BITS/) {	
		if (defined $arg{$_}) {		
			$param->{$_} = $arg{$_};
		} 		 
	} 
			
	foreach (qw /SHOW_HEAD SHOW_DATE SHOW_PEDFILE SHOW_HAPLOFILE SHOW_QUEST 
	             GITTER_X GITTER_Y X_SPACE Y_SPACE CROSS_LOOP ALIVE_SPACE
	             SYMBOL_SIZE CONSANG_DIST LINE_WIDTH  TITLE HAPLO_TEXT_LW
	             LEGEND_SHIFT_LEFT LEGEND_SHIFT_RIGHT MARKER_POS_SHIFT ALLELES_SHIFT
	             LINE_COLOR FAMILY HAPLO_UNKNOWN HAPLO_UNKNOWN_COLOR
	             SHOW_HAPLO_TEXT SHOW_HAPLO_BAR SHOW_HAPLO_NI_0  SHOW_HAPLO_NI_1 SHOW_HAPLO_NI_2
	             SHOW_HAPLO_NI_3 HAPLO_SEP_BL FILL_HAPLO HAPLO_WIDTH HAPLO_WIDTH_NI HAPLO_SPACE
	             HAPLO_LW SHOW_MARKER SHOW_POSITION SHOW_LEGEND_LEFT SHOW_LEGEND_RIGHT 
	             SHOW_COLORED_TEXT ALIGN_LEGEND BACKGROUND 
	             /) {
		if (defined $arg{$_}) {	
			$self->{$_} = $arg{$_};
		}	
	}	
		
	if (defined $arg{AFF_COLOR_0}) { $self->{AFF_COLOR}{0} = $arg{AFF_COLOR_0} }
	if (defined $arg{AFF_COLOR_1}) { $self->{AFF_COLOR}{1} = $arg{AFF_COLOR_1} }
	if (defined $arg{AFF_COLOR_2}) { $self->{AFF_COLOR}{2} = $arg{AFF_COLOR_2} }
	
	if (defined $arg{FONT1_FAMILY}) { 
		$self->{FONT1}{FAMILY} = $arg{FONT1_FAMILY};
		$self->{FONT_HEAD}{FAMILY} = $arg{FONT1_FAMILY};  
	}
	if (defined $arg{FONT1_SIZE})   { $self->{FONT1}{SIZE} = $arg{FONT1_SIZE} }
	if (defined $arg{FONT1_WEIGHT}) { $self->{FONT1}{WEIGHT} = $arg{FONT1_WEIGHT} }
	if (defined $arg{FONT1_SLANT})  { $self->{FONT1}{SLANT} = $arg{FONT1_SLANT} }
	if (defined $arg{FONT1_COLOR})  { $self->{FONT1}{COLOR} = $arg{FONT1_COLOR} }
		
	foreach (qw /PEDIGREE_PATH PEDIGREE_FORMAT EXPORT_PATH/) {
		if (defined $arg{$_}) {	
			$self->{$_} = $arg{$_};
		} else { BatchError("You must declare $_ !") }
	}
	
	if ($arg{HAPLO_PATH} && $arg{HAPLO_FORMAT}) {	
		$self->{HAPLO_PATH} = $arg{HAPLO_PATH};
		$self->{HAPLO_FORMAT} = $arg{HAPLO_FORMAT};
		
	}		
	if ($arg{MAP_PATH}) {
		$self->{MAP_PATH} = $arg{MAP_PATH}
	}
}


#=================
sub BatchProcess {
#=================	
	ReadPed(-file => $self->{PEDIGREE_PATH},-format => $self->{PEDIGREE_FORMAT}) or exit;	
	
	if ($self->{MAP_PATH}) {
		ReadMap(-file => $self->{MAP_PATH}, -format => 1) or exit;
		$self->{HAPLO}{MAP} = \%map;
	}
	
	if ($self->{HAPLO_PATH}) {
		ReadHaplo(-file => $self->{HAPLO_PATH}, -format => $self->{HAPLO_FORMAT}) or exit;
		if ($self->{FAMILY} && $haplo{$self->{FAMILY}}) {
			$self->{HAPLO} = $haplo{$self->{FAMILY}};
			$self->{HAPLO}{MAP} = \%map;
		}
	}
						
	ProcessFamily() or exit;
	FindLoops();
	LoopBreak();
	FindTop() or exit;
	BuildStruk();
	CheckPedigree() or exit;
	DuplicateHaplotypes();
	ShuffleFounderColors();
	ProcessHaplotypes();
	DrawPed();				
	Export();
	exit;
}

#=================
sub DuplicatePid {
#=================	
	my ($p, $mate) = @_;					
	
	$self->{LOOP}{DUPLICATION_COUNTER}{$p}++;
	my $dupc = $self->{LOOP}{DUPLICATION_COUNTER}{$p} + 1;
	my $pn = "$p($dupc)";
	
	$self->{DUPLICATED_PID}{$p}{$pn} = 1;         
	$self->{DUPLICATED_PID_ORIG}{$pn} = $p;
		
	my $k1 = $p . '__' . $mate;
	my $k2 = $pn . '__' . $mate;
	
	my @children = keys % {$self->{CHILDREN_COUPLE}{$p}{$mate} };
	
	delete $self->{COUPLE}{$p}{$mate};
	delete $self->{COUPLE}{$p} unless keys % {$self->{COUPLE}{$p}} ;
	delete $self->{COUPLE}{$mate}{$p};
	delete $self->{COUPLE}{$mate} unless keys % {$self->{COUPLE}{$mate}} ;		
	delete $self->{CHILDREN_COUPLE}{$p}{$mate};
	delete $self->{CHILDREN_COUPLE}{$p} unless keys %{$self->{CHILDREN_COUPLE}{$p}};		
	delete $self->{CHILDREN_COUPLE}{$mate}{$p};
	delete $self->{CHILDREN_COUPLE}{$mate} unless keys %{$self->{CHILDREN_COUPLE}{$mate}};		
	delete $self->{SIBS}{$k1};
				
	foreach (@children) {			
		$self->{SIBS}{$k2}{$_} = 1;			
		$self->{CHILDREN_COUPLE}{$mate}{$pn}{$_} = 1;
		$self->{CHILDREN_COUPLE}{$pn}{$mate}{$_} = 1;
		
		if ($self->{SID2SEX}{$p} == 1) { $self->{SID2FATHER}{$_} = $pn }
		else { $self->{SID2MOTHER}{$_} = $pn }
				
		delete $self->{CHILDREN}{$p}{$_} if $self->{CHILDREN}{$p}{$_};
		$self->{CHILDREN}{$pn}{$_} = 1;
	}
		
	delete $self->{CHILDREN}{$p} unless keys % {$self->{CHILDREN}{$p}};
	
	$self->{SID2SEX}{$pn} = $self->{SID2SEX}{$p};
	$self->{SID2AFF}{$pn} = $self->{SID2AFF}{$p};
	$self->{COUPLE}{$mate}{$pn} = 1;
	$self->{COUPLE}{$pn}{$mate} = 1;
	$self->{PID}{$pn}=1;
	$self->{SID2ALIVE}{$pn} = $self->{SID2ALIVE}{$p} if $self->{SID2ALIVE}{$p};
	
	if (keys % { $self->{CASE_INFO}{PID}{$p} }) {
		foreach (keys % { $self->{CASE_INFO}{PID}{$p} }) {
			$self->{CASE_INFO}{PID}{$pn}{$_} = $self->{CASE_INFO}{PID}{$p}{$_}
		}
		$self->{CASE_INFO}{PID}{$pn}{Case_Info_1} = $pn;
				
	}
	
	if (keys % { $self->{LOOP}{CONSANGUINE}{$p} }) {
		@_ = keys % {$self->{LOOP}{CONSANGUINE}{$p} };
		foreach (@_) {
			$self->{LOOP}{CONSANGUINE}{$pn}{$_} = 1;
			$self->{LOOP}{CONSANGUINE}{$_}{$pn} = 1;
		}
	}		
}

#==============
sub LoopBreak {
#==============	
	if ($param->{LOOP_BREAK_STATUS}) {				
		foreach my $loop (keys % { $self->{LOOP}{NR2END} }) {
			next if ($param->{LOOP_BREAK_STATUS} == 1) && sprintf "%1.0f", rand(1);
			my @p = keys % { $self->{LOOP}{NR2END}{$loop} };
			
			### check and delete if one of the mates belongs to {DROP_CHILDREN_FROM} derived from asymetric loops
			foreach (@p) { delete $self->{LOOP}{DROP_CHILDREN_FROM}{$_} if $self->{LOOP}{DROP_CHILDREN_FROM}{$_} }
			
			ChangeOrder(\@p);
			if (scalar @p > 2) {				
				my $p = shift @p;
				L:foreach (@p) {
					if ($self->{COUPLE}{$p}{$_}) {
						@p = ($p, $_); 
						last L;
					}
				}				
			}
			### set loop break point only if it has not been already set by other loops
			### this could happen if small loops are part of bigger loops		
			$self->{LOOP}{BREAK}{$p[0]}{$p[1]} = 1 if ! $self->{LOOP}{BREAK}{$p[1]}{$p[0]};						
		}				
	}
		
	foreach my $p (keys % { $self->{LOOP}{BREAK} }) {
		foreach my $mate (keys % { $self->{LOOP}{BREAK}{$p} }) {					
			DuplicatePid($p, $mate);
		}				
	}	
}

#===============
sub BatchError {
#===============

	print shift @_ if @_;
	print "\n\nUsage: perl HaploPainter.pl <INFILE>\n\nDemonstration of input parameters\n";
	print '
PEDIGREE_PATH=C:\tmp\pedfile.pre
PEDIGREE_FORMAT=[PRAEMAKEPED_PLUS PRAEMAKEPED POSTMAKEPED]
GRAPHIC_FORMAT=[jpeg png16m png pngalpha postscript pdfwrite ] 
TEXT_ALPHA_BITS=2
GRAPHICS_ALPHA_BITS=2
RESOLUTION=300
BACKGROUND=#ffffff
FAMILY=100
TITLE=This is pedigree 100
EXPORT_PATH=C:\tmp\pedigree100.ps
SHOW_HEAD=1
SHOW_DATE=0
SHOW_PEDFILE=0
GITTER_X=25
GITTER_Y=25
SHOW_QUEST=1
SYMBOL_SIZE=25
BORDER_UP=50
BORDER_DOWN=50
BORDER_LEFT=50
BORDER_RIGHT=50
LINE_COLOR=red
AFF_COLOR_0=#456789
AFF_COLOR_1=yellow
AFF_COLOR_2=brown
FONT1_FAMILY=arial
FONT1_SIZE=20
FONT1_WEIGHT=bold
FONT1_SLANT=roman
FONT1_COLOR=green
';
	
	exit
}

##################################################################################
##################################################################################


__DATA__

Last modification: 29.05.2007


Usage
**********
To draw pedigrees with haplotypes and marker information:

1. File->Import Pedigrees

Praemakeped format (LINKAGE format) has to start with columns separated by white space. Further columns will be ignored, so most files in linkage format are accepted
 
FAMILY_ID
SAMPLE_ID
FATHER_ID
MOTHER_ID
SEX
AFFECTION_STATUS 
... [ignored]
 
Post Makeped format is what the name suggest - the output from the makeped program. You may find it useful for coding the pedigree person IDs and allele numbers, but remember that loops will be broken by duplication of persons. This will result in errors, so don't use this format when your pedigree consists of loops!
 
When finished, the first pedigree from the file is drawn or the program does something strange. The other families are accessible from the View->Draw Family menu.
 
Prae-Makeped-Plus format is the combination of PRAEMAKEPED format and case information. First columns are the same like Prae-Makeped format except for the separator which is *tab key*. After them some new columns for live status and sampleID information may follow. The number of info columns is unlimited, but only 3 additionally columns can be choosen for drawing at once. The order is selectable from the Options -> configuration -> case info menu
 
FAMILY_ID
SAMPLE_ID
FATHER_ID
MOTHER_ID
SEX
AFFECTION_STATUS 
LIVE_STATUS
INFO_1
INFO_2
INFO_3
...
INFO_X


Import pedigrees from database
**********************************************

To import pedigrees from databases follow the File->ImportPedigrees->Database menue.
The database connection window consists of these fields:

Database type: Pop up menue for database type specific drivers
Hostname: name of the server host
Port: used port - defaults are listed
Table Name: name of the table
DBname(SID): name of the database (SID in Oracle)
Username: username
Password: ...

The relation must have the same field order used in Prae-Makeped-Plus format!
At least the first 6 fields are expected. There is no maximum limit in the number of further INFO columns. From the Option->Configuration->Case Info menue you can set up to 4 columns at once to bee drawn in your pedigree. After successful connection you have to label one or more pedigrees from the next Listbox for import.

2. File->Import Haplotypes

At the moment four haplotypes generating programs are supported
 
2.1 Genehunter -> load haplo.dump file
2.2 SimWalk -> load HAPLO.??? file
2.3 Merlin -> load merlin.chr file (vertical orientation)
2.4 Allegro -> load haplo.out, ihaplo.out or founder.out
 
Supplementary information such recombination events are ignored at this state. Instead HaploPainter will perform further haplotype analysis.
 
Rules for haplotype drawing are :
* Finding out the first marker from p telomer from which the phase can be derived and back tracing the haplotype with the color from given phase at the chromosomal starting point.
* The first marker showing differences is declared as the point of recombination and the color is changed to the recombinant haplotype until the next recombination event occurs. Be careful the 'real' point of recombination may be surrounded by uninformative markers. Colored bars are like suggestive traps - region of interests should be checked and manually corrected ! I have warned you!
* Missing genotypes are interpreted as uninformative
* Other uninformative genotypes are drawn in special thin blocks when set in options
	

3. File->Import Map File

There are two supported map file formats differing in a switch of column 2 and 3

Format 1 (CHR-POS-MARKER)
This format was formerly called Mega2 

CHROMOSOM   
POSITION     
MARKER
... [ignored]

Format 2 (CHR-MARKER-POS)
This format is conform with merlin map input file

CHROMOSOM
MARKER
POSITION     
... [ignored]



Once, all information are loaded in, they appear in the drawing window. Now you can play around with different drawing styles available from the option menus. Try it out!

While moving the mouse pointer over uninformative alleles the color is changing. You can double click at the allele and manually change the phase (maternal/paternal/not-informative). From the configuration menu a check button, selective hiding user defined changes, is selectable.

Saving pedigrees as HaploPainter specific format is recommended in case of lots of pedigree modifications use File->Save or File->Save as ... 

Default parameters can be saved/restored. use File->Save Default as .../File->Open Defaults 

A double click at symbols opens a dialog box wherefrom affection and vital status can be changed.

Export and Printing
******************************

You can export the current drawings in different formats from the File->Export ... -> Current Pedigree ... menu or all at once (-> All Pedigrees ... menu)

jpeg (JPG)
png16m (PNG)
pngalpha (PNG + Background information in Alpha channel)
pdfwrite (PDF)

Postscript is the only native supported graphics format. Others are assisted by help of 'Ghostscript' which has to be installed before. Direct printing from Windows systems is possible but demand further installation of 'GSview' (gsprint). GSview and Ghostscript can both be obtained from this site: http://www.cs.wisc.edu/~ghost/

After installation of GSview and Ghostscript you must manually modify the system or user PATH variable

Windows 2000, XP
***************************
1. Right-click on the My Computer icon. (Under Windows XP, the My Computer Icon may be located in the start menu.) 

2. Choose Properties from the context menu.

(Alternatively, you can double-click on the System icon in the Control Panel)

4. Click the Advanced tab.

5. Click the Environment Variables button.

6. Add the target directory to the end of the Path using a semi-colon as a separator.
(If no user PATH variable exists you have to create one)

7. Your PATH variable should look something like
%PATH%;C:\Program Files\gs\gs8.54\bin;C:\Program Files\Ghostgum\gsview; <other paths ...>

Also try getting information from these pages:
http://support.microsoft.com/default.aspx?scid=kb;en-us;310519&sd=tech
http://www.ats.ucla.edu/stat/hlm/faq/path.htm

Linux
*********
If you are a Linux user you probably are familiar with paths. Haplopainter expects having 'gs' inside path environment pointing to the ghostscript executable. Printing from Linux systems depends on installation of GtkLP (http://gtklp.sourceforge.net/)


Drag and Drop
*********************
Some drag and drop features like moving symbols and title are implemented.
For easy zooming also try shift/contr. + left mouse button.


Command Line
********************
For using HaploPainter in command line mode you have to create a parameter input file first. 
Usage is >perl HaploPainter[version].pl <INPUT FILE> 

To see some currently supported parameters start haplopainter with -h switch. It is possible to integrate Haplopainter in a web server in command mode but, therefore an active X-server must running in background.

Further on line help is available: http://haplopainter.sourceforge.net

HaploPainter is open source software and anybody is invited to participate in the project! 
Please send any bugs and comments to: hthiele@users.sourceforge.net

Good luck...

Holger Thiele

