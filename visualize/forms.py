from django import forms
class ContactForm(forms.Form):		

	data_choices = (
		('Trajectory1','Trajectory1'), ('Trajectory2','Trajectory2'), ('Trajectory3','Trajectory3'),
	)

	min_distance = (
		('1','1'),('2',2),
	)
	group_members = (
		('1',1),('2',2),('3',3),
	)
	time_choices = (
		('1',1),('2',2),('3',3), 
	)
	data_field = forms.ChoiceField(choices=data_choices, required=True, label='Trajectory Data')
	time_field = forms.ChoiceField(choices=time_choices, required=True, label='Minimum Time Together')
	members_field = forms.ChoiceField(choices=group_members, required=True, label='Minimum Group Members')
	distance_field = forms.ChoiceField(choices=min_distance, required=True, label='Min Distance Together')
	# the new bit we're adding
	def __init__(self, *args, **kwargs):
			super(ContactForm, self).__init__(*args, **kwargs)
			
