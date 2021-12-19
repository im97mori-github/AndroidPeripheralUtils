package org.im97mori.ble.android.peripheral.ui.device.type;

import androidx.annotation.NonNull;
import androidx.core.util.Pair;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.datasource.DeviceDataSource;

import java.util.List;

import javax.inject.Inject;

public class DeviceTypeListViewModel extends ViewModel {

    @Inject
    DeviceDataSource mDeviceDataSource;

    @NonNull
    public List<Pair<Integer, String>> getDeviceTypeList() {
        return mDeviceDataSource.provideDeviceTypeList();
    }

}