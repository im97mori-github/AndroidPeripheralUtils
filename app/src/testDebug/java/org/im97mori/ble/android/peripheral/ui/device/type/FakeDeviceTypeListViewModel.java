package org.im97mori.ble.android.peripheral.ui.device.type;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;

@HiltViewModel
public class FakeDeviceTypeListViewModel extends DeviceTypeListViewModel {

    @Inject
    FakeDeviceTypeListViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
    }

}