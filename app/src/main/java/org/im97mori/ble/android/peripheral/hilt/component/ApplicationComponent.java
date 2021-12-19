package org.im97mori.ble.android.peripheral.hilt.component;

import org.im97mori.ble.android.peripheral.ui.device.PeripheralActivity;
import org.im97mori.ble.android.peripheral.ui.device.PeripheralViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a23.SystemIdSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a23.SystemIdSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a24.ModelNumberStringSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a24.ModelNumberStringSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a29.ManufacturerNameStringSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a29.ManufacturerNameStringSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a35.BloodPressureMeasurementSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a35.BloodPressureMeasurementSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a36.IntermediateCuffPressureSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a36.IntermediateCuffPressureSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a49.BloodPressureFeatureSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a49.BloodPressureFeatureSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceTypeListActivity;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceTypeListViewModel;
import org.im97mori.ble.android.peripheral.ui.main.MainActivity;
import org.im97mori.ble.android.peripheral.ui.main.MainViewModel;

//@Singleton
//@Component(modules = {
//        ApplicationModule.class
//        , DeviceModule.class
//        , TextModule.class
//})
public interface ApplicationComponent {
    void inject(MainActivity activity);

    void inject(MainViewModel viewModel);

    void inject(DeviceTypeListActivity activity);

    void inject(DeviceTypeListViewModel viewModel);

    void inject(DeviceSettingActivity activity);

    void inject(DeviceSettingViewModel viewModel);

    void inject(BloodPressureProfileViewModel viewModel);

    void inject(DeviceInformationServiceSettingActivity activity);

    void inject(DeviceInformationServiceSettingViewModel viewModel);

    void inject(BloodPressureServiceSettingActivity activity);

    void inject(BloodPressureServiceSettingViewModel viewModel);

    void inject(ManufacturerNameStringSettingActivity activity);

    void inject(ManufacturerNameStringSettingViewModel viewModel);

    void inject(ModelNumberStringSettingActivity activity);

    void inject(ModelNumberStringSettingViewModel viewModel);

    void inject(SystemIdSettingActivity activity);

    void inject(SystemIdSettingViewModel viewModel);

    void inject(BloodPressureMeasurementSettingActivity activity);

    void inject(BloodPressureMeasurementSettingViewModel viewModel);

    void inject(IntermediateCuffPressureSettingActivity activity);

    void inject(IntermediateCuffPressureSettingViewModel viewModel);

    void inject(BloodPressureFeatureSettingActivity activity);

    void inject(BloodPressureFeatureSettingViewModel viewModel);

    void inject(ClientCharacteristicConfigurationSettingActivity activity);

    void inject(ClientCharacteristicConfigurationSettingViewModel viewModel);

    void inject(PeripheralActivity activity);

    void inject(PeripheralViewModel viewModel);

}