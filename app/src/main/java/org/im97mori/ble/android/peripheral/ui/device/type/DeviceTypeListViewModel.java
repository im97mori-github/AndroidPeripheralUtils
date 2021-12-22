package org.im97mori.ble.android.peripheral.ui.device.type;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;

@HiltViewModel
public class DeviceTypeListViewModel extends ViewModel {

    private final DeviceRepository mDeviceRepository;

    @Inject
    public DeviceTypeListViewModel(@NonNull DeviceRepository deviceRepository) {
        mDeviceRepository = deviceRepository;
    }

    @NonNull
    public List<Pair<Integer, String>> getDeviceTypeList() {
        return mDeviceRepository.provideDeviceTypeList();
    }

    @NonNull
    public Map<Integer, Integer> provideDeviceTypeImageResMap() {
        return mDeviceRepository.provideDeviceTypeImageResMap();
    }

}