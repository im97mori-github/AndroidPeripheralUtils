package org.im97mori.ble.android.peripheral.ui.device.type;

import android.content.Intent;
import android.os.Bundle;
import android.widget.ListView;

import androidx.lifecycle.ViewModelProvider;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.AndroidPeripheralUtilsApplication;
import org.im97mori.ble.android.peripheral.ui.BaseActivity;

public class DeviceTypeListActivity extends BaseActivity {

    public static final String KEY_DEVICE_TYPE = "KEY_DEVICE_TYPE";

    private DeviceTypeListAdapter adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        mApplicationComponent = ((AndroidPeripheralUtilsApplication) getApplication()).getComponent();

        mApplicationComponent.inject(this);
        super.onCreate(savedInstanceState);
        DeviceTypeListViewModel viewModel = new ViewModelProvider(this).get(DeviceTypeListViewModel.class);
        mApplicationComponent.inject(viewModel);

        setContentView(R.layout.device_type_list_activity);

        ListView listView = findViewById(R.id.list);
        adapter = new DeviceTypeListAdapter(this, viewModel.getDeviceTypeList());
        listView.setAdapter(adapter);
        listView.setOnItemClickListener((parent, view, position, id) -> {
            Intent intent = new Intent();
            intent.putExtra(KEY_DEVICE_TYPE, adapter.getItem(position).first);
            setResult(RESULT_OK, intent);
            finish();
        });
    }

}