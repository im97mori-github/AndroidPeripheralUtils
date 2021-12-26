package org.im97mori.ble.android.peripheral.ui.device.type;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;

@HiltViewModel
public class DeviceTypeListViewModel extends ViewModel {

    private final DeviceSettingRepository mDeviceSettingRepository;

    @Inject
    public DeviceTypeListViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        mDeviceSettingRepository = deviceSettingRepository;
    }

    @NonNull
    public List<Pair<Integer, String>> provideDeviceTypeList() {
        return mDeviceSettingRepository.provideDeviceTypeList();
    }

    @NonNull
    public Map<Integer, Integer> provideDeviceTypeImageResMap() {
        return mDeviceSettingRepository.provideDeviceTypeImageResMap();
    }

}