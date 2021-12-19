package org.im97mori.ble.android.peripheral.ui.device.setting;

import androidx.lifecycle.ViewModel;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.datasource.ResourceTextSource;

import javax.inject.Inject;

public abstract class BaseSettingViewModel extends ViewModel {

    @Inject
    public Gson mGson;

    @Inject
    public ResourceTextSource mResourceTextSource;

}