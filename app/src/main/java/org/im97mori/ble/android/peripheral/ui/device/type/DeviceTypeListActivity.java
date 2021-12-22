package org.im97mori.ble.android.peripheral.ui.device.type;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

import android.content.Intent;
import android.os.Bundle;

import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.databinding.DeviceTypeListActivityBinding;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class DeviceTypeListActivity extends BaseActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        DeviceTypeListViewModel viewModel = new ViewModelProvider(this).get(DeviceTypeListViewModel.class);
        DeviceTypeListActivityBinding binding = DeviceTypeListActivityBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        DeviceTypeListAdapter adapter = new DeviceTypeListAdapter(this, viewModel.provideDeviceTypeImageResMap(), viewModel.getDeviceTypeList());
        binding.list.setAdapter(adapter);
        binding.list.setOnItemClickListener((parent, view, position, id) -> {
            Intent intent = new Intent();
            intent.putExtra(KEY_DEVICE_TYPE, adapter.getItem(position).first);
            setResult(RESULT_OK, intent);
            finish();
        });
    }

}