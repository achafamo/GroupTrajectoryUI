"""
Definition of urls for Compgeo_project.
"""

from django.conf.urls import include, url
from visualize.views import home, get_trajectories
# Uncomment the next two lines to enable the admin:
# from django.contrib import admin
# admin.autodiscover()

urlpatterns = [
    # Examples:
    url(r'^$', home),
    url(r'trajectorydata/$', get_trajectories),
    # url(r'^Compgeo_project/', include('Compgeo_project.Compgeo_project.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # url(r'^admin/', include(admin.site.urls)),
]
